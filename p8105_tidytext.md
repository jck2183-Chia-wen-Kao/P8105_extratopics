p8105\_Tidy Text
================

## Get Data

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_reviews = 
  tibble(
    page = 1:100,
    urls = str_c(url_base, page)) %>% 
  mutate(reviews = map(urls, read_page_reviews)) %>% 
  unnest(reviews) %>%
  mutate(review_num = row_number()) %>% 
  relocate(page, review_num)
```

## Do some tidy text stuff

``` r
dynamite_words =
    dynamite_reviews %>% 
    unnest_tokens(word, text) %>% 
    select(-urls)
```

remove boring words

``` r
dynamite_words =
    anti_join(dynamite_words, stop_words)
```

    ## Joining, by = "word"

Usual Tidyverse stuff…

``` r
dynamite_words %>% 
    count(word, sort = TRUE) %>% 
    top_n(10) %>% 
    mutate(word = fct_reorder(word, n)) %>% 
    ggplot(aes(x = word, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
```

    ## Selecting by n

<img src="p8105_tidytext_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />
