## Week 8

# Today we use the heterogeneous MNL from week 6 to assess whether we should
# hire a celebrity to affiliate with our brand.



library(tidyverse)
library(mlogit)


# Suppose we are Samsung, like last week, with the same market size and marginal costs.
# We are considering hiring @Khaby.Lame of TikTok fame to be a celebrity 
# spokesperson for our brand. Should we do this?


# load data, fit mnl model

    # load main dataset
    load("../data/mnl_datasets.RData")
    
    # get the product names for the 6 products
    prod_vec <- c("A1", "A2", "S1", "S2", "H1", "H2")

    # fit mnl data
    out <- mlogit(choice ~ apple:segment + 
                           samsung:segment + 
                           price:segment +
                           screen_size:segment + 
                           price:total_minutes:segment | 0, data=mdat1)
    
    
# calculate expected profit without celebrity affiliation
    
    # expected/predicted product-specific market shares
    pred1 <- colMeans(predict(out, newdata = mdat1))
    names(pred1) <- prod_vec
    round(pred1*100, 1)
    
    # calculate baseline prices and product-specific profits
    d1 <- tibble(price1 = 799, 
                 share1 = pred1[3],
                 price2 = 899,
                 share2 = pred1[4])
    
    # assumed market size
    M <- 10
    
    # calculate quantities and revenues
    d1 <- d1 %>% mutate(q1 = share1*M, 
                        q2 = share2*M,
                        rev1 = q1 * price1,
                        rev2 = q2 * price2)
    
    # marginal cost for phones S1 and s2
    mc1 <- 440
    mc2 <- 470
    
    # calculate costs and profits
    d1 <- d1 %>% mutate(cost1 = mc1 * q1,
                        cost2 = mc2 * q2,
                        profit1 = rev1 - cost1,
                        profit2 = rev2 - cost2,
                        total_profit = profit1 + profit2)
    
    # print the result
    d1
    
    
# calculate pure demand effect of association with a celebrity

    # suppose we learn from other market research that a celebrity like 
    # @Khaby.Lame will improve our brand perception by an amount delta.
    # And suppose we have done some work to convert delta into a "CCE"
    # that is, a celebrity coefficient effect of size 0.005
    
    # celebrity coefficient effect
    cce <- 0.005
    
    # let's create an adjusted version of our model where we increase the 
    # samsung brand-intercept coefficients by the amount of the CCE
    out_adj <- out
    out_adj$coefficients[4:6] <- out_adj$coefficients[4:6] + cce
    
    # let's look at the original coefficients next to the adjusted ones, where
    # we'll see that they are the same except the samsung:segment intercepts
    # which are 0.005 higher
    cbind(coef(out), coef(out_adj))
    
    # calculate revised predictions from this adjusted model
    pred2 <- colMeans(predict(out_adj, newdata = mdat1))
    names(pred2) <- prod_vec
    
    # calculate change in market shares
    shr_change <- pred2 - pred1
    names(shr_change) <- prod_vec
    
    # print original expected market shares, now with-celeb market shares,
    # and their difference.  Notice that we multiple by 100, so the interpretation
    # is that affiliation with Kaby leads to a 0.073% increase for the S1 phone and
    # a 0.038% increase for the S2 phone
    round(pred1*100, 2)
    round(pred2*100, 2)
    round(shr_change*100, 3)
    
    # let's plot a comparison of market shares with and without Kaby affiliation.
    # You'll see the heights of the bars are very close.
    pdat <- rbind(
        tibble(celeb="No",  product=prod_vec, share=pred1),
        tibble(celeb="Yes", product=prod_vec, share=pred2)
    )
    
    pdat <- pdat %>% mutate(celeb=fct_inorder(factor(celeb)))
    ggplot(pdat) +
        geom_col(aes(product, share, fill=celeb), position="dodge") + 
        ggtitle("Pure Demand Effect") +
        ylab("Product-Specific Market Shares") + 
        xlab("Product") + 
        scale_fill_manual("Celebrity", values=c(No="Firebrick", Yes="Dodgerblue4")) + 
        theme_bw()
    
    
    # calculate new expected profit
    d2 <- tibble(price1 = 799, 
                 share1 = pred2[3],
                 price2 = 899,
                 share2 = pred2[4])
    
    # calculate quantities, revenues, costs, and profits
    d2 <- d2 %>% mutate(q1 = share1*M, 
                        q2 = share2*M,
                        rev1 = q1 * price1,
                        rev2 = q2 * price2,
                        cost1 = mc1 * q1,
                        cost2 = mc2 * q2,
                        profit1 = rev1 - cost1,
                        profit2 = rev2 - cost2,
                        total_profit = profit1 + profit2)
    
    # print the result
    rbind(d1, d2)
    
    # calculate the change in profit
    d2$total_profit - d1$total_profit
    
    # We see that hiring @Khaby.Lame leads to an increase in profit of $4.23 million, so
    # if he charges any amount less than that, it is profitable for Samsung to hire him.
    
    
# Find new optimal prices
    
    # Let's assume we hire Kaby and then optimize prices for the S1 and S2 phones.
    
    # this mimics what we did last week, but instead of searching for the best price
    # for one phone, we will search for the best price for both Samsung phones at the
    # same time
    
    # calculate market shares under hypothetical prices
    
        # get a vector of price changes to use for each phone
        pvec <- seq(from=-150, to=150, by=10)
        
        # get all combinations of price changes for the two phones
        res <- expand.grid(pvec, pvec)
        
        # and construct empty matrix to store shares at each price
        smat <- matrix(NA_real_, nrow=nrow(res), ncol=6)
        colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")
        
        res <- cbind(res, smat)
        
        # loop over the 961 price change values
        for(i in 1:nrow(res)) {
            
            # print progress
            cat("Working on", i, "of", nrow(res), "\n")
            
            # get the price change amount
            p1 <- res[i,1]
            p2 <- res[i,2]
            
            # change prices for S1 phones
            tempdat <- as_tibble(mdat1)
            tempdat <- tempdat %>% mutate(price = ifelse(phone_id == "S1", price + p1, price))
            tempdat <- tempdat %>% mutate(price = ifelse(phone_id == "S2", price + p2, price))
            
            # make market share predictions with the temporarily-changed S1 and S2 prices
            preds <- predict(out_adj, newdata=tempdat)
            
            # calculate and store market shares
            res[i,3:8] <- colMeans(preds)
        }
    
    # gather prices and estimated shares into a dataframe
    d3 <- tibble(scenario = 1:nrow(res), 
                   price1 = res[,1] + 799, 
                   share1 = res[,5],
                   price2 = res[,2] + 899,
                   share2 = res[,6])
    
    # calculate quantities, revenues, costs, and profits
    d3 <- d3 %>% mutate(q1 = share1*M, 
                        q2 = share2*M,
                        rev1 = q1 * price1,
                        rev2 = q2 * price2,
                        cost1 = mc1 * q1,
                        cost2 = mc2 * q2,
                        profit1 = rev1 - cost1,
                        profit2 = rev2 - cost2,
                        total_profit = profit1 + profit2)
    
    # plot heat map of profit
    ggplot(d3) + 
        geom_tile(aes(x=price1, y=price2, fill=total_profit)) + 
        scale_fill_gradient(low="white", high="blue") + 
        xlab("S1 Price") + 
        ylab("S2 Price") + 
        ggtitle("Profit Heat Map by Product Pricing") +
        geom_point(data=d3 %>% filter(total_profit == max(total_profit)), aes(price1, price2), color="white", size=5) +
        geom_point(data=d2, aes(price1, price2), color="red", size=5) +
        theme_bw()
    
    # select profit-maximizing price combination and compare to prior calculations
    d3 <- d3 %>% filter(total_profit == max(total_profit)) %>% select(-scenario)
    rbind(d1, d2, d3)
    
    # we see that the model predicts a "baseline" $1.347 billion in Samsung profit.
    # If Samsung hire Kaby, the profit increases to $1.351 billion (from which we 
    # need to subtract what Kaby charges us).  
    # And if Samsung hires Kaby and optimizes prices, w expect profit of $1.435 billion.
    
    # calculate change in profit from price optimization
    d3$total_profit - d2$total_profit
    
    
# Two major comments:
    
    # 1. The total amount a company should be willing to pay to hire a celebrity 
    #    should be derived under an assumption of optimal pricing. 
    
    # 2. We should also take into account the possibility that a competitor hires
    #    the celebrity, likely resulting in that competitor stealing some of our 
    #    market share and profits.
    
    # So, a "more correct" analysis would find the difference between:
    # (A) the good scenario where we hire the celeb and optimize prices, versus
    # (B) the bad scenario where a competitor hires the celeb.
    
    # The difference between Scenarios A and B is the most we should be willing to pay
    # to hire the celeb.  Note that it is possible to estimate this difference even if 
    # we assume there is only some probability (e.g., 60%) that a competitor would hire
    # the celeb if we do not.
    

    
    
    
# Summary of R commands introduced
    
    # expand.grid()  -- creates all combinations of values from two vectors
    
    
    
    
    
    