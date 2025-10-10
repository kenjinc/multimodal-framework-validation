Cleaning and Analysis
================

## Packages

``` r
library(ggpubr)
```

    ## Loading required package: ggplot2

``` r
library(ggsignif)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(viridis)
```

    ## Loading required package: viridisLite

## Data

``` r
sales_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/daily-sales-counts.csv") 
foot_traffic_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/daily-sales-traffic.csv")
emissions_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/parent-data/item-kg-co2e.csv")
water_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/parent-data/item-l-h2o-blue.csv")
historical_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/parent-data/historical-transaction-volume.csv")
```

## Cleaning (Global)

### Inconsistencies between sales categories and stations

The first item we’ll have to address involves the discrepancies in how
items are categorized within the point-of-sale (POS) system and the
physical dining environment.

In particular, the “Asian,” “Grill,” and “Italian” sales categories, as
they are represented within `sales_data` by the POS system, are—in
reality—each representing two independent station concepts: the “Ramen”
and “Wok” concepts for the “Asian” category, the “Grill” and
“Quesadilla” concepts for the “Grill” category, and the “Pasta” and
“Pizza” concepts for the “Italian” category. We will also make
corresponding changes to the “Mexican” sales category, which flags sales
from the “Wrap” station concept.

To help us incorporate this information, we’ll first use the following
code chunks to tease out both the names and number of unique items
listed under each of the nine sales categories.

``` r
sales_data %>%
  group_by(sales_cat) %>%
  distinct(item) %>%
  arrange(sales_cat,item) %>%
  head(5)
```

    ## # A tibble: 5 × 2
    ## # Groups:   sales_cat [1]
    ##   sales_cat item               
    ##   <chr>     <chr>              
    ## 1 Asian     1 Entree + 1 Side  
    ## 2 Asian     1 Entree + 2 Side  
    ## 3 Asian     1 Wok Entree       
    ## 4 Asian     2 Entrees + 2 Sides
    ## 5 Asian     Add Extra Toppings

``` r
sales_data %>%
  group_by(sales_cat) %>%
  distinct(item) %>%
  count()
```

    ## # A tibble: 9 × 2
    ## # Groups:   sales_cat [9]
    ##   sales_cat     n
    ##   <chr>     <int>
    ## 1 Asian        14
    ## 2 Breakfast    14
    ## 3 Deli          1
    ## 4 Grab N Go     6
    ## 5 Grill        20
    ## 6 Italian       6
    ## 7 Mexican       6
    ## 8 Salad Bar     3
    ## 9 Soup          2

With these, we are able to easily reference the information needed to
perform two initial checks: one to see whether there are identical
items—be them modifications, mains, or sides—with the same name sold
under different sales categories and another to see if there are
identical items with different names sold under the same sales category.

``` r
sales_data %>%
  group_by(sales_cat) %>%
  distinct(item) %>%
  arrange(item,sales_cat) 
```

    ## # A tibble: 72 × 2
    ## # Groups:   sales_cat [9]
    ##    sales_cat item                     
    ##    <chr>     <chr>                    
    ##  1 Asian     1 Entree + 1 Side        
    ##  2 Asian     1 Entree + 2 Side        
    ##  3 Asian     1 Wok Entree             
    ##  4 Asian     2 Entrees + 2 Sides      
    ##  5 Breakfast 2 Slices Toast           
    ##  6 Soup      8 oz Soup                
    ##  7 Grill     ADD Beef Patty           
    ##  8 Grill     ADD Beef Patty $2.99     
    ##  9 Grill     ADD Burger Salmon Grilled
    ## 10 Grill     ADD Cheese               
    ## # ℹ 62 more rows

From these initial checks, we can tease out a few things:

- The modification used to indicate whether a guest added a beef patty
  to their order is represented differently under the “Grill” sales
  category depending on whether the sale was made during Fall 2024
  (i.e., “ADD Beef Patty \$2.99”) or Spring 2025 (i.e., “ADD Beef
  Patty”).

- “Side Potato Tots” is listed with the same name under both the “Grill”
  sales category and the “Grab N Go” sales category. During Fall 2024,
  “Side Potato Tots” sales were categorized under the “Grab N Go” sales
  category whereas, during Spring 2025, they were categorized under
  “Grill.”

Furthermore, from our understanding of how the listed items are
physically arranged within the dining context, we can also use these
checks to determine whether there are items that are represented
consistently both within and across sales categories but filed under a
category other than the one that where they are sold.

- “Fried Chicken Tenders” are categorized under the “Grill” sales
  category despite being sold alongside the items listed under “Grab N
  Go.” Because part of our analysis relies on understanding how diners
  choose between main, side, and modification options at the “Grill,”
  specifically, we will instead opt to file sales of “Fried Chicken
  Tenders” under “Grab N Go.”

Given this, we’ll make the following changes for consistency:

- We will adjust the item name for the beef-patty modification so that
  it is consistent across the two study periods (i.e., “+ Beef Patty”).

``` r
sales_data$item <- gsub("\\$", "",sales_data$item)
sales_data <- sales_data %>%
  mutate(item=str_replace_all(item,"ADD Beef Patty 2.99","+ Beef Patty")) %>%
  mutate(item=str_replace_all(item,"ADD Beef Patty","+ Beef Patty"))
```

- We will categorize “Side Potato Tots” and “Fried Chicken Tenders”
  under the “Grab N Go” sales category across both study periods.

``` r
sales_data <- sales_data %>% 
  mutate(sales_cat=case_when(item=="Side Potato Tots"~"Grab N Go",
                             TRUE ~ sales_cat)) %>%
  mutate(sales_cat=case_when(item=="Fried Chicken Tenders"~"Grab N Go",
                             TRUE ~ sales_cat))
```

With these initial checks performed, and the prescribed changes
addressed, we can now move on to resolve the inconsistencies between the
sales categories created via the POS system and the station concepts
where those items are actually sold. We will accomplish this by creating
a new categorical variable, `station`, that pairs items according to the
stations where they are sold within the physical dining environment.

``` r
sales_data %>%
  group_by(sales_cat) %>%
  distinct(item) %>%
  arrange(sales_cat,item) 
```

    ## # A tibble: 70 × 2
    ## # Groups:   sales_cat [9]
    ##    sales_cat item                       
    ##    <chr>     <chr>                      
    ##  1 Asian     1 Entree + 1 Side          
    ##  2 Asian     1 Entree + 2 Side          
    ##  3 Asian     1 Wok Entree               
    ##  4 Asian     2 Entrees + 2 Sides        
    ##  5 Asian     Add Extra Toppings         
    ##  6 Asian     Bowl Ramen Chicken         
    ##  7 Asian     Bowl Ramen Pork            
    ##  8 Asian     Bowl Ramen Tofu            
    ##  9 Asian     Side Fried Spring Roll     
    ## 10 Asian     Side Vegetable Spring Rolls
    ## # ℹ 60 more rows

``` r
sales_data <- sales_data %>% 
  mutate(station=case_when(item=="1 Entree + 1 Side"~"Wok",
                           item=="1 Entree + 2 Side"~"Wok",
                           item=="1 Wok Entree"~"Wok",
                           item=="2 Entrees + 2 Sides"~"Wok",
                           item=="Add Extra Toppings"~"Ramen",
                           item=="Bowl Ramen Chicken"~"Ramen",
                           item=="Bowl Ramen Pork"~"Ramen",
                           item=="Bowl Ramen Tofu"~"Ramen",
                           item=="Side Fried Spring Roll"~"Wok",
                           item=="Side Vegetable Spring Rolls"~"Wok",
                           item=="Side Vegetables"~"Wok",
                           item=="Side Vegetarian Fried Rice with"~"Wok",
                           item=="Side Vegetarian Lo Mein"~"Wok",
                           item=="Side White or Brown Rice"~"Wok",
                           item=="2 Slices Toast"~"Breakfast",
                           item=="Add Bacon"~"Breakfast",
                           item=="Burrito Breakfast"~"Breakfast",
                           item=="Egg Cheese Bacon Breakfast Sandw"~"Breakfast",
                           item=="Egg Cheese Sausage Breakfast San"~"Breakfast",
                           item=="Grand Slam Breakfast"~"Breakfast",
                           item=="PC Butter"~"Breakfast",
                           item=="PC Jelly"~"Breakfast",
                           item=="PC Peanut Butter"~"Breakfast",
                           item=="Pancake Single"~"Breakfast",
                           item=="Small French Omelet"~"Breakfast",
                           item=="Toast"~"Breakfast",
                           item=="Trillium Home Fries"~"Breakfast",
                           item=="Two Eggs"~"Breakfast",
                           item=="LTO Sandwich"~"Deli",
                           item=="Burrito Breakfast G&G"~"Grab N Go",
                           item=="Egg Cheese Bacon Breakfast Sandwich"~"Grab N Go",
                           item=="Egg Cheese Sausage Breakfast Sandwich"~"Grab N Go",
                           item=="LTO Meatball Sub"~"Grab N Go",
                           item=="LTO Spicy Chicken Sandwich"~"Grab N Go",
                           item=="Side Potato Tots"~"Grab N Go",
                           item=="+ Beef Patty"~"Grill",
                           item=="ADD Burger Salmon Grilled"~"Grill",
                           item=="ADD Cheese"~"Grill",
                           item=="ADD Chicken Breast"~"Grill",
                           item=="Add Egg .99"~"Grill",
                           item=="Add Impossible Burger Patty"~"Grill",
                           item=="Add Sausage 2 Patty"~"Grill",
                           item=="Black Bean Burger"~"Grill",
                           item=="Burrito Una Mano Trillium BYO"~"Quesadilla",
                           item=="French Fries"~"Grill",
                           item=="Fried Chicken Tenders"~"Grab N Go",
                           item=="Grilled Chicken Breast Sandwich"~"Grill",
                           item=="Grilled Hamburger"~"Grill",
                           item=="Quesadilla Cheese"~"Quesadilla",
                           item=="Quesadilla Deluxe Trillium"~"Quesadilla",
                           item=="Seared Salmon Burger"~"Grill",
                           item=="Sweet Potato Fries"~"Grill",
                           item=="Trillium Grill Impossible Burger"~"Grill",
                           item=="Add Extra Meat"~"Pasta",
                           item=="Create Your Pasta Bowl MEAT"~"Pasta",
                           item=="Create Your Pasta Bowl VEG"~"Pasta",
                           item=="Pizza Cheese"~"Pizza",
                           item=="Pizza with Toppings"~"Pizza",
                           item=="Side Bread Pasta Station"~"Pasta",
                           item=="Add Extra Toppings Una Mano"~"Wrap",
                           item=="Burrito Bowl BYO"~"Wrap",
                           item=="Side Guacamole"~"Wrap",
                           item=="Side Salsa"~"Wrap",
                           item=="Side Sour Cream"~"Wrap",
                           item=="Single Taco"~"Wrap",
                           item=="Add Extra Protein 2.99"~"Salad Bar",
                           item=="Add Extra Protein 3.99"~"Salad Bar",
                           item=="Salad by the Pound"~"Salad Bar",
                           item=="8 oz Soup"~"Soup",
                           item=="Soup 12 oz"~"Soup"))
```

Then, we perform a few quick checks to ensure all of the items were
coded as intended, without duplicates or typos.

``` r
sum(is.na(sales_data$station))
```

    ## [1] 0

``` r
sales_data %>%
  distinct(station)
```

    ##       station
    ## 1  Quesadilla
    ## 2       Grill
    ## 3   Grab N Go
    ## 4         Wok
    ## 5       Ramen
    ## 6       Pasta
    ## 7       Pizza
    ## 8   Breakfast
    ## 9        Wrap
    ## 10  Salad Bar
    ## 11       Soup
    ## 12       Deli

### Correcting known input errors

For the next stage of our cleaning process, we corresponded with our
research partners at the dining facility to clarify how we ought to
treat the sales of items that were noticeably absent from the previously
reviewed dining menus. In particular, we noted that, at the “Ramen”
station, there were recorded sales of “Bowl Ramen Pork,” despite this
not being an advertised offering within the dining facility during
either of the two study periods.

Our site partners noted that these were likely input errors that
occurred at the register, in part because it was a previously programmed
option to accommodate sales of this item during a prior time period.
They correspondingly advised that we should map those purchases onto
sales of “Bowl Ramen Chicken,” as opposed to “Bowl Ramen Tofu,” on the
basis that it was more likely that a sales assistant would visually
misidentify chicken as pork—as opposed to tofu-at check-out.

As such, we make the following change:

``` r
sales_data <- sales_data %>% 
  mutate(item=str_replace_all(item,"Bowl Ramen Pork","Bowl Ramen Chicken")) 
```

### Distinguishing between mains, modifications, and sides

In the interest of separating our analysis between the different types
of items listed within `sales_data`, we will create a new categorical
variable, `item_cat` specifying each sales tally according to whether
the item references the sale of a side, a main, or a modification made
to a main.

``` r
sales_data <- sales_data %>%
  mutate(item_cat=case_when(item=="1 Entree + 1 Side"~"Main",
                           item=="1 Entree + 2 Side"~"Main",
                           item=="1 Wok Entree"~"Main",
                           item=="2 Entrees + 2 Sides"~"Main",
                           item=="Add Extra Toppings"~"Modification",
                           item=="Bowl Ramen Chicken"~"Main",
                           item=="Bowl Ramen Pork"~"Main",
                           item=="Bowl Ramen Tofu"~"Main",
                           item=="Side Fried Spring Roll"~"Side",
                           item=="Side Vegetable Spring Rolls"~"Side",
                           item=="Side Vegetables"~"Side",
                           item=="Side Vegetarian Fried Rice with"~"Side",
                           item=="Side Vegetarian Lo Mein"~"Side",
                           item=="Side White or Brown Rice"~"Side",
                           item=="2 Slices Toast"~"Side",
                           item=="Add Bacon"~"Modification",
                           item=="Burrito Breakfast"~"Main",
                           item=="Egg Cheese Bacon Breakfast Sandw"~"Main",
                           item=="Egg Cheese Sausage Breakfast San"~"Main",
                           item=="Grand Slam Breakfast"~"Main",
                           item=="PC Butter"~"Side",
                           item=="PC Jelly"~"Side",
                           item=="PC Peanut Butter"~"Side",
                           item=="Pancake Single"~"Side",
                           item=="Small French Omelet"~"Side",
                           item=="Toast"~"Side",
                           item=="Trillium Home Fries"~"Side",
                           item=="Two Eggs"~"Side",
                           item=="LTO Sandwich"~"Main",
                           item=="Burrito Breakfast G&G"~"Main",
                           item=="Egg Cheese Bacon Breakfast Sandwich"~"Main",
                           item=="Egg Cheese Sausage Breakfast Sandwich"~"Main",
                           item=="LTO Meatball Sub"~"Main",
                           item=="LTO Spicy Chicken Sandwich"~"Main",
                           item=="Side Potato Tots"~"Side",
                           item=="+ Beef Patty"~"Modification",
                           item=="ADD Burger Salmon Grilled"~"Modification",
                           item=="ADD Cheese"~"Modification",
                           item=="ADD Chicken Breast"~"Modification",
                           item=="Add Egg .99"~"Modification",
                           item=="Add Impossible Burger Patty"~"Modification",
                           item=="Add Sausage 2 Patty"~"Modification",
                           item=="Black Bean Burger"~"Main",
                           item=="Burrito Una Mano Trillium BYO"~"Main",
                           item=="French Fries"~"Side",
                           item=="Fried Chicken Tenders"~"Side",
                           item=="Grilled Chicken Breast Sandwich"~"Main",
                           item=="Grilled Hamburger"~"Main",
                           item=="Quesadilla Cheese"~"Main",
                           item=="Quesadilla Deluxe Trillium"~"Main",
                           item=="Seared Salmon Burger"~"Main",
                           item=="Sweet Potato Fries"~"Side",
                           item=="Trillium Grill Impossible Burger"~"Main",
                           item=="Add Extra Meat"~"Modification",
                           item=="Create Your Pasta Bowl MEAT"~"Main",
                           item=="Create Your Pasta Bowl VEG"~"Main",
                           item=="Pizza Cheese"~"Main",
                           item=="Pizza with Toppings"~"Main",
                           item=="Side Bread Pasta Station"~"Side",
                           item=="Add Extra Toppings Una Mano"~"Modification",
                           item=="Burrito Bowl BYO"~"Main",
                           item=="Side Guacamole"~"Side",
                           item=="Side Salsa"~"Side",
                           item=="Side Sour Cream"~"Side",
                           item=="Single Taco"~"Main",
                           item=="Add Extra Protein 2.99"~"Modification",
                           item=="Add Extra Protein 3.99"~"Modification",
                           item=="Salad by the Pound"~"Main",
                           item=="8 oz Soup"~"Main",
                           item=="Soup 12 oz"~"Main"))
write.csv(sales_data,"/Users/kenjinchang/github/multimodal-framework-validation/data/cleaned_daily-sales-traffic.csv")
```

As we did before, we will perform a series of checks to ensure the new
variable was coded as intended.

``` r
sum(is.na(sales_data$item_cat))
```

    ## [1] 0

``` r
sales_data %>%
  distinct(item_cat)
```

    ##       item_cat
    ## 1         Main
    ## 2         Side
    ## 3 Modification

### Distinguishing between meal periods and treatment and non-treatment stations

We use the following code chunks to, first, list out each of the
relevant stations and, subsequently, designate them as either satellite
(i.e., used to measure satellite effects associated with implementing
the intervention, such as between-station spillover) or
treatment-receiving (i.e.,used to measure within-station changes in food
choice).

``` r
sales_data %>%
  distinct(station) %>%
  arrange(station)
```

    ##       station
    ## 1   Breakfast
    ## 2        Deli
    ## 3   Grab N Go
    ## 4       Grill
    ## 5       Pasta
    ## 6       Pizza
    ## 7  Quesadilla
    ## 8       Ramen
    ## 9   Salad Bar
    ## 10       Soup
    ## 11        Wok
    ## 12       Wrap

``` r
sales_data <- sales_data %>%
  mutate(station_type=case_when(station=="Breakfast"~"Satellite",
                                station=="Deli"~"Satellite",
                                station=="Grab N Go"~"Satellite",
                                station=="Grill"~"Treatment",
                                station=="Pasta"~"Satellite",
                                station=="Pizza"~"Satellite",
                                station=="Quesadilla"~"Satellite",
                                station=="Ramen"~"Treatment",
                                station=="Salad Bar"~"Satellite",
                                station=="Soup"~"Satellite",
                                station=="Wok"~"Satellite",
                                station=="Wrap"~"Satellite")) 
write.csv(sales_data,"/Users/kenjinchang/github/multimodal-framework-validation/data/cleaned_daily-sales-traffic.csv")
```

Furthermore,

``` r
sales_data %>%
  mutate(meal_period=case_when(station=="Breakfast"~"Satellite",
                                station=="Deli"~"Satellite",
                                station=="Grab N Go"~"Satellite",
                                station=="Grill"~"Treatment",
                                station=="Pasta"~"Satellite",
                                station=="Pizza"~"Satellite",
                                station=="Quesadilla"~"Satellite",
                                station=="Ramen"~"Treatment",
                                station=="Salad Bar"~"Satellite",
                                station=="Soup"~"Satellite",
                                station=="Wok"~"Satellite",
                                station=="Wrap"~"Satellite")) 
```

    ##        date                                  item count sales_cat    semester
    ## 1    16-Oct            Quesadilla Deluxe Trillium   140     Grill   Fall 2024
    ## 2    16-Oct                     Grilled Hamburger    91     Grill   Fall 2024
    ## 3    16-Oct                 Fried Chicken Tenders    92 Grab N Go   Fall 2024
    ## 4    16-Oct         Burrito Una Mano Trillium BYO    62     Grill   Fall 2024
    ## 5    16-Oct                          French Fries   113     Grill   Fall 2024
    ## 6    16-Oct                     Quesadilla Cheese    35     Grill   Fall 2024
    ## 7    16-Oct       Grilled Chicken Breast Sandwich    14     Grill   Fall 2024
    ## 8    16-Oct                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 9    16-Oct      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 10   16-Oct                    Sweet Potato Fries    21     Grill   Fall 2024
    ## 11   16-Oct                          + Beef Patty    13     Grill   Fall 2024
    ## 12   16-Oct                     Black Bean Burger     2     Grill   Fall 2024
    ## 13   16-Oct           Add Impossible Burger Patty     1     Grill   Fall 2024
    ## 14   16-Oct                           Add Egg .99     6     Grill   Fall 2024
    ## 15   16-Oct                            ADD Cheese     9     Grill   Fall 2024
    ## 16   16-Oct                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 17   16-Oct             ADD Burger Salmon Grilled     1     Grill   Fall 2024
    ## 18   16-Oct                     1 Entree + 1 Side   206     Asian   Fall 2024
    ## 19   16-Oct                     1 Entree + 2 Side    79     Asian   Fall 2024
    ## 20   16-Oct                    Bowl Ramen Chicken    83     Asian   Fall 2024
    ## 21   16-Oct                   2 Entrees + 2 Sides    20     Asian   Fall 2024
    ## 22   16-Oct                       Bowl Ramen Tofu    16     Asian   Fall 2024
    ## 23   16-Oct                    Bowl Ramen Chicken     5     Asian   Fall 2024
    ## 24   16-Oct               Side Vegetarian Lo Mein     6     Asian   Fall 2024
    ## 25   16-Oct                Side Fried Spring Roll     4     Asian   Fall 2024
    ## 26   16-Oct              Side White or Brown Rice     4     Asian   Fall 2024
    ## 27   16-Oct           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 28   16-Oct                          1 Wok Entree     1     Asian   Fall 2024
    ## 29   16-Oct                       Side Vegetables     1     Asian   Fall 2024
    ## 30   16-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 31   16-Oct           Create Your Pasta Bowl MEAT   149   Italian   Fall 2024
    ## 32   16-Oct            Create Your Pasta Bowl VEG    24   Italian   Fall 2024
    ## 33   16-Oct                          Pizza Cheese    28   Italian   Fall 2024
    ## 34   16-Oct                        Add Extra Meat    34   Italian   Fall 2024
    ## 35   16-Oct                   Pizza with Toppings    16   Italian   Fall 2024
    ## 36   16-Oct              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 37   16-Oct                     Burrito Breakfast    87 Breakfast   Fall 2024
    ## 38   16-Oct                   Small French Omelet    53 Breakfast   Fall 2024
    ## 39   16-Oct      Egg Cheese Sausage Breakfast San    34 Breakfast   Fall 2024
    ## 40   16-Oct      Egg Cheese Bacon Breakfast Sandw    26 Breakfast   Fall 2024
    ## 41   16-Oct                  Grand Slam Breakfast     9 Breakfast   Fall 2024
    ## 42   16-Oct                             Add Bacon    26 Breakfast   Fall 2024
    ## 43   16-Oct                              Two Eggs    10 Breakfast   Fall 2024
    ## 44   16-Oct                        Pancake Single     4 Breakfast   Fall 2024
    ## 45   16-Oct                        2 Slices Toast     4 Breakfast   Fall 2024
    ## 46   16-Oct                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 47   16-Oct                      Burrito Bowl BYO   105   Mexican   Fall 2024
    ## 48   16-Oct                           Single Taco     3   Mexican   Fall 2024
    ## 49   16-Oct                       Side Sour Cream     1   Mexican   Fall 2024
    ## 50   16-Oct                    Salad by the Pound    79 Salad Bar   Fall 2024
    ## 51   16-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 52   16-Oct                            Soup 12 oz    67      Soup   Fall 2024
    ## 53   16-Oct                             8 oz Soup    60      Soup   Fall 2024
    ## 54   16-Oct                      Side Potato Tots    12 Grab N Go   Fall 2024
    ## 55   17-Oct            Quesadilla Deluxe Trillium   159     Grill   Fall 2024
    ## 56   17-Oct                     Grilled Hamburger   109     Grill   Fall 2024
    ## 57   17-Oct                 Fried Chicken Tenders   109 Grab N Go   Fall 2024
    ## 58   17-Oct         Burrito Una Mano Trillium BYO    51     Grill   Fall 2024
    ## 59   17-Oct                          French Fries   136     Grill   Fall 2024
    ## 60   17-Oct       Grilled Chicken Breast Sandwich    22     Grill   Fall 2024
    ## 61   17-Oct      Trillium Grill Impossible Burger    12     Grill   Fall 2024
    ## 62   17-Oct                     Quesadilla Cheese    13     Grill   Fall 2024
    ## 63   17-Oct                    Sweet Potato Fries    27     Grill   Fall 2024
    ## 64   17-Oct                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 65   17-Oct                          + Beef Patty    19     Grill   Fall 2024
    ## 66   17-Oct                     Black Bean Burger     1     Grill   Fall 2024
    ## 67   17-Oct                            ADD Cheese    10     Grill   Fall 2024
    ## 68   17-Oct                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 69   17-Oct                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 70   17-Oct                           Add Egg .99     1     Grill   Fall 2024
    ## 71   17-Oct                     1 Entree + 1 Side   189     Asian   Fall 2024
    ## 72   17-Oct                     1 Entree + 2 Side    81     Asian   Fall 2024
    ## 73   17-Oct                    Bowl Ramen Chicken    68     Asian   Fall 2024
    ## 74   17-Oct                   2 Entrees + 2 Sides    35     Asian   Fall 2024
    ## 75   17-Oct                       Bowl Ramen Tofu    14     Asian   Fall 2024
    ## 76   17-Oct                          1 Wok Entree     5     Asian   Fall 2024
    ## 77   17-Oct           Side Vegetable Spring Rolls     3     Asian   Fall 2024
    ## 78   17-Oct               Side Vegetarian Lo Mein     3     Asian   Fall 2024
    ## 79   17-Oct                       Side Vegetables     2     Asian   Fall 2024
    ## 80   17-Oct              Side White or Brown Rice     2     Asian   Fall 2024
    ## 81   17-Oct           Create Your Pasta Bowl MEAT   128   Italian   Fall 2024
    ## 82   17-Oct            Create Your Pasta Bowl VEG    33   Italian   Fall 2024
    ## 83   17-Oct                   Pizza with Toppings    37   Italian   Fall 2024
    ## 84   17-Oct                          Pizza Cheese    27   Italian   Fall 2024
    ## 85   17-Oct                        Add Extra Meat    24   Italian   Fall 2024
    ## 86   17-Oct              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 87   17-Oct                     Burrito Breakfast    85 Breakfast   Fall 2024
    ## 88   17-Oct                   Small French Omelet    61 Breakfast   Fall 2024
    ## 89   17-Oct      Egg Cheese Sausage Breakfast San    36 Breakfast   Fall 2024
    ## 90   17-Oct      Egg Cheese Bacon Breakfast Sandw    34 Breakfast   Fall 2024
    ## 91   17-Oct                  Grand Slam Breakfast    15 Breakfast   Fall 2024
    ## 92   17-Oct                             Add Bacon    28 Breakfast   Fall 2024
    ## 93   17-Oct                              Two Eggs    17 Breakfast   Fall 2024
    ## 94   17-Oct                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 95   17-Oct                        Pancake Single     1 Breakfast   Fall 2024
    ## 96   17-Oct                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 97   17-Oct                                 Toast     1 Breakfast   Fall 2024
    ## 98   17-Oct                      Burrito Bowl BYO   103   Mexican   Fall 2024
    ## 99   17-Oct                           Single Taco     4   Mexican   Fall 2024
    ## 100  17-Oct                        Side Guacamole     4   Mexican   Fall 2024
    ## 101  17-Oct                            Side Salsa     3   Mexican   Fall 2024
    ## 102  17-Oct           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 103  17-Oct                    Salad by the Pound    57 Salad Bar   Fall 2024
    ## 104  17-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 105  17-Oct                             8 oz Soup    35      Soup   Fall 2024
    ## 106  17-Oct                            Soup 12 oz    28      Soup   Fall 2024
    ## 107  17-Oct                      Side Potato Tots    18 Grab N Go   Fall 2024
    ## 108  18-Oct            Quesadilla Deluxe Trillium   105     Grill   Fall 2024
    ## 109  18-Oct                     Grilled Hamburger    66     Grill   Fall 2024
    ## 110  18-Oct                 Fried Chicken Tenders    54 Grab N Go   Fall 2024
    ## 111  18-Oct         Burrito Una Mano Trillium BYO    42     Grill   Fall 2024
    ## 112  18-Oct                          French Fries   101     Grill   Fall 2024
    ## 113  18-Oct                     Quesadilla Cheese    14     Grill   Fall 2024
    ## 114  18-Oct       Grilled Chicken Breast Sandwich    11     Grill   Fall 2024
    ## 115  18-Oct                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 116  18-Oct                          + Beef Patty    11     Grill   Fall 2024
    ## 117  18-Oct      Trillium Grill Impossible Burger     2     Grill   Fall 2024
    ## 118  18-Oct                     Black Bean Burger     2     Grill   Fall 2024
    ## 119  18-Oct                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 120  18-Oct                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 121  18-Oct                            ADD Cheese     4     Grill   Fall 2024
    ## 122  18-Oct                           Add Egg .99     1     Grill   Fall 2024
    ## 123  18-Oct                     1 Entree + 1 Side   103     Asian   Fall 2024
    ## 124  18-Oct                     1 Entree + 2 Side    63     Asian   Fall 2024
    ## 125  18-Oct                    Bowl Ramen Chicken    44     Asian   Fall 2024
    ## 126  18-Oct                   2 Entrees + 2 Sides    20     Asian   Fall 2024
    ## 127  18-Oct                       Bowl Ramen Tofu    21     Asian   Fall 2024
    ## 128  18-Oct              Side White or Brown Rice    12     Asian   Fall 2024
    ## 129  18-Oct               Side Vegetarian Lo Mein     4     Asian   Fall 2024
    ## 130  18-Oct                          1 Wok Entree     2     Asian   Fall 2024
    ## 131  18-Oct           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 132  18-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 133  18-Oct                     Burrito Breakfast    84 Breakfast   Fall 2024
    ## 134  18-Oct                   Small French Omelet    47 Breakfast   Fall 2024
    ## 135  18-Oct      Egg Cheese Sausage Breakfast San    36 Breakfast   Fall 2024
    ## 136  18-Oct                  Grand Slam Breakfast    15 Breakfast   Fall 2024
    ## 137  18-Oct      Egg Cheese Bacon Breakfast Sandw    23 Breakfast   Fall 2024
    ## 138  18-Oct                             Add Bacon    21 Breakfast   Fall 2024
    ## 139  18-Oct                              Two Eggs    10 Breakfast   Fall 2024
    ## 140  18-Oct                        Pancake Single     6 Breakfast   Fall 2024
    ## 141  18-Oct                   Trillium Home Fries     2 Breakfast   Fall 2024
    ## 142  18-Oct                              PC Jelly     1 Breakfast   Fall 2024
    ## 143  18-Oct           Create Your Pasta Bowl MEAT    83   Italian   Fall 2024
    ## 144  18-Oct                   Pizza with Toppings    23   Italian   Fall 2024
    ## 145  18-Oct                          Pizza Cheese    27   Italian   Fall 2024
    ## 146  18-Oct            Create Your Pasta Bowl VEG    13   Italian   Fall 2024
    ## 147  18-Oct                        Add Extra Meat    14   Italian   Fall 2024
    ## 148  18-Oct                      Burrito Bowl BYO    81   Mexican   Fall 2024
    ## 149  18-Oct                           Single Taco     4   Mexican   Fall 2024
    ## 150  18-Oct                             8 oz Soup    39      Soup   Fall 2024
    ## 151  18-Oct                            Soup 12 oz    24      Soup   Fall 2024
    ## 152  18-Oct                    Salad by the Pound    34 Salad Bar   Fall 2024
    ## 153  18-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 154  18-Oct                      Side Potato Tots    15 Grab N Go   Fall 2024
    ## 155  21-Oct            Quesadilla Deluxe Trillium   154     Grill   Fall 2024
    ## 156  21-Oct                     Grilled Hamburger   105     Grill   Fall 2024
    ## 157  21-Oct         Burrito Una Mano Trillium BYO    79     Grill   Fall 2024
    ## 158  21-Oct                 Fried Chicken Tenders    91 Grab N Go   Fall 2024
    ## 159  21-Oct                          French Fries   114     Grill   Fall 2024
    ## 160  21-Oct       Grilled Chicken Breast Sandwich    18     Grill   Fall 2024
    ## 161  21-Oct                    Sweet Potato Fries    36     Grill   Fall 2024
    ## 162  21-Oct                  Seared Salmon Burger    11     Grill   Fall 2024
    ## 163  21-Oct      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 164  21-Oct                     Quesadilla Cheese     6     Grill   Fall 2024
    ## 165  21-Oct                          + Beef Patty    14     Grill   Fall 2024
    ## 166  21-Oct                    ADD Chicken Breast     4     Grill   Fall 2024
    ## 167  21-Oct                   Add Sausage 2 Patty     5     Grill   Fall 2024
    ## 168  21-Oct                     Black Bean Burger     1     Grill   Fall 2024
    ## 169  21-Oct                            ADD Cheese     4     Grill   Fall 2024
    ## 170  21-Oct                           Add Egg .99     2     Grill   Fall 2024
    ## 171  21-Oct                     1 Entree + 1 Side   187     Asian   Fall 2024
    ## 172  21-Oct                     1 Entree + 2 Side    87     Asian   Fall 2024
    ## 173  21-Oct                    Bowl Ramen Chicken    71     Asian   Fall 2024
    ## 174  21-Oct                   2 Entrees + 2 Sides    20     Asian   Fall 2024
    ## 175  21-Oct                       Bowl Ramen Tofu    14     Asian   Fall 2024
    ## 176  21-Oct               Side Vegetarian Lo Mein     7     Asian   Fall 2024
    ## 177  21-Oct           Side Vegetable Spring Rolls     5     Asian   Fall 2024
    ## 178  21-Oct                          1 Wok Entree     3     Asian   Fall 2024
    ## 179  21-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 180  21-Oct              Side White or Brown Rice     1     Asian   Fall 2024
    ## 181  21-Oct           Create Your Pasta Bowl MEAT   124   Italian   Fall 2024
    ## 182  21-Oct            Create Your Pasta Bowl VEG    37   Italian   Fall 2024
    ## 183  21-Oct                   Pizza with Toppings    38   Italian   Fall 2024
    ## 184  21-Oct                          Pizza Cheese    23   Italian   Fall 2024
    ## 185  21-Oct                        Add Extra Meat    22   Italian   Fall 2024
    ## 186  21-Oct              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 187  21-Oct                     Burrito Breakfast    80 Breakfast   Fall 2024
    ## 188  21-Oct                   Small French Omelet    51 Breakfast   Fall 2024
    ## 189  21-Oct      Egg Cheese Sausage Breakfast San    32 Breakfast   Fall 2024
    ## 190  21-Oct                  Grand Slam Breakfast    16 Breakfast   Fall 2024
    ## 191  21-Oct      Egg Cheese Bacon Breakfast Sandw    28 Breakfast   Fall 2024
    ## 192  21-Oct                             Add Bacon    35 Breakfast   Fall 2024
    ## 193  21-Oct                              Two Eggs    15 Breakfast   Fall 2024
    ## 194  21-Oct                   Trillium Home Fries     6 Breakfast   Fall 2024
    ## 195  21-Oct                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 196  21-Oct                                 Toast     2 Breakfast   Fall 2024
    ## 197  21-Oct                      Burrito Bowl BYO   101   Mexican   Fall 2024
    ## 198  21-Oct                           Single Taco     7   Mexican   Fall 2024
    ## 199  21-Oct                        Side Guacamole     6   Mexican   Fall 2024
    ## 200  21-Oct           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 201  21-Oct                    Salad by the Pound    59 Salad Bar   Fall 2024
    ## 202  21-Oct                            Soup 12 oz    43      Soup   Fall 2024
    ## 203  21-Oct                             8 oz Soup    27      Soup   Fall 2024
    ## 204  21-Oct                      Side Potato Tots    14 Grab N Go   Fall 2024
    ## 205  22-Oct            Quesadilla Deluxe Trillium   161     Grill   Fall 2024
    ## 206  22-Oct                     Grilled Hamburger   127     Grill   Fall 2024
    ## 207  22-Oct                 Fried Chicken Tenders   120 Grab N Go   Fall 2024
    ## 208  22-Oct         Burrito Una Mano Trillium BYO    62     Grill   Fall 2024
    ## 209  22-Oct                          French Fries   146     Grill   Fall 2024
    ## 210  22-Oct       Grilled Chicken Breast Sandwich    18     Grill   Fall 2024
    ## 211  22-Oct                    Sweet Potato Fries    34     Grill   Fall 2024
    ## 212  22-Oct                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 213  22-Oct                  Seared Salmon Burger    10     Grill   Fall 2024
    ## 214  22-Oct      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 215  22-Oct                          + Beef Patty     9     Grill   Fall 2024
    ## 216  22-Oct                     Black Bean Burger     2     Grill   Fall 2024
    ## 217  22-Oct                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 218  22-Oct                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 219  22-Oct                           Add Egg .99     2     Grill   Fall 2024
    ## 220  22-Oct                            ADD Cheese     2     Grill   Fall 2024
    ## 221  22-Oct                     1 Entree + 1 Side   197     Asian   Fall 2024
    ## 222  22-Oct                     1 Entree + 2 Side    87     Asian   Fall 2024
    ## 223  22-Oct                    Bowl Ramen Chicken    73     Asian   Fall 2024
    ## 224  22-Oct                   2 Entrees + 2 Sides    36     Asian   Fall 2024
    ## 225  22-Oct                       Bowl Ramen Tofu    10     Asian   Fall 2024
    ## 226  22-Oct               Side Vegetarian Lo Mein     4     Asian   Fall 2024
    ## 227  22-Oct                          1 Wok Entree     1     Asian   Fall 2024
    ## 228  22-Oct              Side White or Brown Rice     3     Asian   Fall 2024
    ## 229  22-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 230  22-Oct           Create Your Pasta Bowl MEAT   132   Italian   Fall 2024
    ## 231  22-Oct            Create Your Pasta Bowl VEG    26   Italian   Fall 2024
    ## 232  22-Oct                   Pizza with Toppings    35   Italian   Fall 2024
    ## 233  22-Oct                          Pizza Cheese    24   Italian   Fall 2024
    ## 234  22-Oct                        Add Extra Meat    21   Italian   Fall 2024
    ## 235  22-Oct              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 236  22-Oct                     Burrito Breakfast    89 Breakfast   Fall 2024
    ## 237  22-Oct                   Small French Omelet    60 Breakfast   Fall 2024
    ## 238  22-Oct                  Grand Slam Breakfast    19 Breakfast   Fall 2024
    ## 239  22-Oct      Egg Cheese Sausage Breakfast San    28 Breakfast   Fall 2024
    ## 240  22-Oct      Egg Cheese Bacon Breakfast Sandw    26 Breakfast   Fall 2024
    ## 241  22-Oct                             Add Bacon    27 Breakfast   Fall 2024
    ## 242  22-Oct                              Two Eggs    14 Breakfast   Fall 2024
    ## 243  22-Oct                        Pancake Single     5 Breakfast   Fall 2024
    ## 244  22-Oct                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 245  22-Oct                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 246  22-Oct                                 Toast     2 Breakfast   Fall 2024
    ## 247  22-Oct                              PC Jelly     1 Breakfast   Fall 2024
    ## 248  22-Oct                      Burrito Bowl BYO   113   Mexican   Fall 2024
    ## 249  22-Oct                           Single Taco     3   Mexican   Fall 2024
    ## 250  22-Oct                        Side Guacamole     4   Mexican   Fall 2024
    ## 251  22-Oct           Add Extra Toppings Una Mano     3   Mexican   Fall 2024
    ## 252  22-Oct                    Salad by the Pound    80 Salad Bar   Fall 2024
    ## 253  22-Oct                             8 oz Soup    51      Soup   Fall 2024
    ## 254  22-Oct                            Soup 12 oz    33      Soup   Fall 2024
    ## 255  22-Oct                      Side Potato Tots    14 Grab N Go   Fall 2024
    ## 256  23-Oct            Quesadilla Deluxe Trillium   149     Grill   Fall 2024
    ## 257  23-Oct                     Grilled Hamburger   100     Grill   Fall 2024
    ## 258  23-Oct                 Fried Chicken Tenders   110 Grab N Go   Fall 2024
    ## 259  23-Oct         Burrito Una Mano Trillium BYO    59     Grill   Fall 2024
    ## 260  23-Oct                          French Fries   143     Grill   Fall 2024
    ## 261  23-Oct       Grilled Chicken Breast Sandwich    20     Grill   Fall 2024
    ## 262  23-Oct      Trillium Grill Impossible Burger    10     Grill   Fall 2024
    ## 263  23-Oct                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 264  23-Oct                    Sweet Potato Fries    27     Grill   Fall 2024
    ## 265  23-Oct                          + Beef Patty    15     Grill   Fall 2024
    ## 266  23-Oct                  Seared Salmon Burger     3     Grill   Fall 2024
    ## 267  23-Oct                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 268  23-Oct                     Black Bean Burger     2     Grill   Fall 2024
    ## 269  23-Oct                   Add Sausage 2 Patty     5     Grill   Fall 2024
    ## 270  23-Oct                            ADD Cheese     8     Grill   Fall 2024
    ## 271  23-Oct                           Add Egg .99     1     Grill   Fall 2024
    ## 272  23-Oct                     1 Entree + 1 Side   194     Asian   Fall 2024
    ## 273  23-Oct                     1 Entree + 2 Side    92     Asian   Fall 2024
    ## 274  23-Oct                    Bowl Ramen Chicken    83     Asian   Fall 2024
    ## 275  23-Oct                   2 Entrees + 2 Sides    19     Asian   Fall 2024
    ## 276  23-Oct                       Bowl Ramen Tofu     9     Asian   Fall 2024
    ## 277  23-Oct                          1 Wok Entree     3     Asian   Fall 2024
    ## 278  23-Oct           Side Vegetable Spring Rolls     4     Asian   Fall 2024
    ## 279  23-Oct       Side Vegetarian Fried Rice with     3     Asian   Fall 2024
    ## 280  23-Oct               Side Vegetarian Lo Mein     3     Asian   Fall 2024
    ## 281  23-Oct              Side White or Brown Rice     5     Asian   Fall 2024
    ## 282  23-Oct                       Side Vegetables     2     Asian   Fall 2024
    ## 283  23-Oct           Create Your Pasta Bowl MEAT   124   Italian   Fall 2024
    ## 284  23-Oct            Create Your Pasta Bowl VEG    28   Italian   Fall 2024
    ## 285  23-Oct                   Pizza with Toppings    38   Italian   Fall 2024
    ## 286  23-Oct                          Pizza Cheese    23   Italian   Fall 2024
    ## 287  23-Oct                        Add Extra Meat    25   Italian   Fall 2024
    ## 288  23-Oct              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 289  23-Oct                     Burrito Breakfast    89 Breakfast   Fall 2024
    ## 290  23-Oct                   Small French Omelet    73 Breakfast   Fall 2024
    ## 291  23-Oct      Egg Cheese Bacon Breakfast Sandw    30 Breakfast   Fall 2024
    ## 292  23-Oct      Egg Cheese Sausage Breakfast San    29 Breakfast   Fall 2024
    ## 293  23-Oct                  Grand Slam Breakfast    10 Breakfast   Fall 2024
    ## 294  23-Oct                             Add Bacon    34 Breakfast   Fall 2024
    ## 295  23-Oct                              Two Eggs    16 Breakfast   Fall 2024
    ## 296  23-Oct                   Trillium Home Fries     6 Breakfast   Fall 2024
    ## 297  23-Oct                        Pancake Single     2 Breakfast   Fall 2024
    ## 298  23-Oct                                 Toast     2 Breakfast   Fall 2024
    ## 299  23-Oct                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 300  23-Oct                      Burrito Bowl BYO    92   Mexican   Fall 2024
    ## 301  23-Oct                           Single Taco     5   Mexican   Fall 2024
    ## 302  23-Oct                        Side Guacamole     1   Mexican   Fall 2024
    ## 303  23-Oct           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 304  23-Oct                    Salad by the Pound    77 Salad Bar   Fall 2024
    ## 305  23-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 306  23-Oct                            Soup 12 oz    36      Soup   Fall 2024
    ## 307  23-Oct                             8 oz Soup    35      Soup   Fall 2024
    ## 308  23-Oct                      Side Potato Tots    17 Grab N Go   Fall 2024
    ## 309  24-Oct            Quesadilla Deluxe Trillium   165     Grill   Fall 2024
    ## 310  24-Oct                     Grilled Hamburger   107     Grill   Fall 2024
    ## 311  24-Oct                 Fried Chicken Tenders   107 Grab N Go   Fall 2024
    ## 312  24-Oct         Burrito Una Mano Trillium BYO    66     Grill   Fall 2024
    ## 313  24-Oct                          French Fries   132     Grill   Fall 2024
    ## 314  24-Oct       Grilled Chicken Breast Sandwich    14     Grill   Fall 2024
    ## 315  24-Oct      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 316  24-Oct                    Sweet Potato Fries    31     Grill   Fall 2024
    ## 317  24-Oct                  Seared Salmon Burger    10     Grill   Fall 2024
    ## 318  24-Oct                     Quesadilla Cheese     9     Grill   Fall 2024
    ## 319  24-Oct                     Black Bean Burger     5     Grill   Fall 2024
    ## 320  24-Oct                          + Beef Patty    12     Grill   Fall 2024
    ## 321  24-Oct                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 322  24-Oct                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 323  24-Oct                           Add Egg .99     4     Grill   Fall 2024
    ## 324  24-Oct                            ADD Cheese     4     Grill   Fall 2024
    ## 325  24-Oct                     1 Entree + 1 Side   195     Asian   Fall 2024
    ## 326  24-Oct                     1 Entree + 2 Side    93     Asian   Fall 2024
    ## 327  24-Oct                    Bowl Ramen Chicken    92     Asian   Fall 2024
    ## 328  24-Oct                   2 Entrees + 2 Sides    39     Asian   Fall 2024
    ## 329  24-Oct                       Bowl Ramen Tofu    15     Asian   Fall 2024
    ## 330  24-Oct               Side Vegetarian Lo Mein     7     Asian   Fall 2024
    ## 331  24-Oct                          1 Wok Entree     2     Asian   Fall 2024
    ## 332  24-Oct       Side Vegetarian Fried Rice with     3     Asian   Fall 2024
    ## 333  24-Oct              Side White or Brown Rice     5     Asian   Fall 2024
    ## 334  24-Oct           Create Your Pasta Bowl MEAT   131   Italian   Fall 2024
    ## 335  24-Oct            Create Your Pasta Bowl VEG    32   Italian   Fall 2024
    ## 336  24-Oct                   Pizza with Toppings    39   Italian   Fall 2024
    ## 337  24-Oct                          Pizza Cheese    32   Italian   Fall 2024
    ## 338  24-Oct                        Add Extra Meat    16   Italian   Fall 2024
    ## 339  24-Oct              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 340  24-Oct                     Burrito Breakfast    86 Breakfast   Fall 2024
    ## 341  24-Oct                   Small French Omelet    64 Breakfast   Fall 2024
    ## 342  24-Oct                  Grand Slam Breakfast    27 Breakfast   Fall 2024
    ## 343  24-Oct      Egg Cheese Sausage Breakfast San    37 Breakfast   Fall 2024
    ## 344  24-Oct      Egg Cheese Bacon Breakfast Sandw    32 Breakfast   Fall 2024
    ## 345  24-Oct                             Add Bacon    32 Breakfast   Fall 2024
    ## 346  24-Oct                              Two Eggs    23 Breakfast   Fall 2024
    ## 347  24-Oct                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 348  24-Oct                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 349  24-Oct                        Pancake Single     1 Breakfast   Fall 2024
    ## 350  24-Oct                                 Toast     3 Breakfast   Fall 2024
    ## 351  24-Oct                              PC Jelly     3 Breakfast   Fall 2024
    ## 352  24-Oct                      PC Peanut Butter     2 Breakfast   Fall 2024
    ## 353  24-Oct                    Salad by the Pound    82 Salad Bar   Fall 2024
    ## 354  24-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 355  24-Oct                      Burrito Bowl BYO    78   Mexican   Fall 2024
    ## 356  24-Oct                        Side Guacamole     4   Mexican   Fall 2024
    ## 357  24-Oct                           Single Taco     1   Mexican   Fall 2024
    ## 358  24-Oct           Add Extra Toppings Una Mano     5   Mexican   Fall 2024
    ## 359  24-Oct                            Side Salsa     1   Mexican   Fall 2024
    ## 360  24-Oct                            Soup 12 oz    48      Soup   Fall 2024
    ## 361  24-Oct                             8 oz Soup    41      Soup   Fall 2024
    ## 362  24-Oct                      Side Potato Tots    20 Grab N Go   Fall 2024
    ## 363  25-Oct            Quesadilla Deluxe Trillium   110     Grill   Fall 2024
    ## 364  25-Oct                     Grilled Hamburger    71     Grill   Fall 2024
    ## 365  25-Oct         Burrito Una Mano Trillium BYO    57     Grill   Fall 2024
    ## 366  25-Oct                 Fried Chicken Tenders    59 Grab N Go   Fall 2024
    ## 367  25-Oct                          French Fries    82     Grill   Fall 2024
    ## 368  25-Oct       Grilled Chicken Breast Sandwich     8     Grill   Fall 2024
    ## 369  25-Oct                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 370  25-Oct                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 371  25-Oct      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 372  25-Oct                     Black Bean Burger     4     Grill   Fall 2024
    ## 373  25-Oct                          + Beef Patty     4     Grill   Fall 2024
    ## 374  25-Oct                    ADD Chicken Breast     2     Grill   Fall 2024
    ## 375  25-Oct                            ADD Cheese     4     Grill   Fall 2024
    ## 376  25-Oct                           Add Egg .99     1     Grill   Fall 2024
    ## 377  25-Oct                     1 Entree + 1 Side   105     Asian   Fall 2024
    ## 378  25-Oct                     1 Entree + 2 Side    59     Asian   Fall 2024
    ## 379  25-Oct                    Bowl Ramen Chicken    44     Asian   Fall 2024
    ## 380  25-Oct                   2 Entrees + 2 Sides    19     Asian   Fall 2024
    ## 381  25-Oct                       Bowl Ramen Tofu     9     Asian   Fall 2024
    ## 382  25-Oct               Side Vegetarian Lo Mein     8     Asian   Fall 2024
    ## 383  25-Oct                          1 Wok Entree     4     Asian   Fall 2024
    ## 384  25-Oct       Side Vegetarian Fried Rice with     3     Asian   Fall 2024
    ## 385  25-Oct              Side White or Brown Rice     3     Asian   Fall 2024
    ## 386  25-Oct           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 387  25-Oct                     Burrito Breakfast    80 Breakfast   Fall 2024
    ## 388  25-Oct                   Small French Omelet    51 Breakfast   Fall 2024
    ## 389  25-Oct      Egg Cheese Sausage Breakfast San    33 Breakfast   Fall 2024
    ## 390  25-Oct      Egg Cheese Bacon Breakfast Sandw    30 Breakfast   Fall 2024
    ## 391  25-Oct                  Grand Slam Breakfast    16 Breakfast   Fall 2024
    ## 392  25-Oct                             Add Bacon    18 Breakfast   Fall 2024
    ## 393  25-Oct                              Two Eggs     8 Breakfast   Fall 2024
    ## 394  25-Oct                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 395  25-Oct                        Pancake Single     2 Breakfast   Fall 2024
    ## 396  25-Oct                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 397  25-Oct           Create Your Pasta Bowl MEAT    80   Italian   Fall 2024
    ## 398  25-Oct                   Pizza with Toppings    24   Italian   Fall 2024
    ## 399  25-Oct            Create Your Pasta Bowl VEG    14   Italian   Fall 2024
    ## 400  25-Oct                          Pizza Cheese    11   Italian   Fall 2024
    ## 401  25-Oct                        Add Extra Meat    12   Italian   Fall 2024
    ## 402  25-Oct                      Burrito Bowl BYO    50   Mexican   Fall 2024
    ## 403  25-Oct                           Single Taco     4   Mexican   Fall 2024
    ## 404  25-Oct                        Side Guacamole     1   Mexican   Fall 2024
    ## 405  25-Oct           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 406  25-Oct                    Salad by the Pound    39 Salad Bar   Fall 2024
    ## 407  25-Oct                             8 oz Soup    38      Soup   Fall 2024
    ## 408  25-Oct                            Soup 12 oz    25      Soup   Fall 2024
    ## 409  25-Oct                      Side Potato Tots    27 Grab N Go   Fall 2024
    ## 410  28-Oct            Quesadilla Deluxe Trillium   156     Grill   Fall 2024
    ## 411  28-Oct                     Grilled Hamburger    96     Grill   Fall 2024
    ## 412  28-Oct                 Fried Chicken Tenders    97 Grab N Go   Fall 2024
    ## 413  28-Oct         Burrito Una Mano Trillium BYO    71     Grill   Fall 2024
    ## 414  28-Oct                          French Fries   122     Grill   Fall 2024
    ## 415  28-Oct       Grilled Chicken Breast Sandwich    15     Grill   Fall 2024
    ## 416  28-Oct                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 417  28-Oct                    Sweet Potato Fries    28     Grill   Fall 2024
    ## 418  28-Oct                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 419  28-Oct      Trillium Grill Impossible Burger     4     Grill   Fall 2024
    ## 420  28-Oct                          + Beef Patty    13     Grill   Fall 2024
    ## 421  28-Oct                     Black Bean Burger     4     Grill   Fall 2024
    ## 422  28-Oct                    ADD Chicken Breast     4     Grill   Fall 2024
    ## 423  28-Oct                   Add Sausage 2 Patty     6     Grill   Fall 2024
    ## 424  28-Oct                           Add Egg .99     5     Grill   Fall 2024
    ## 425  28-Oct                            ADD Cheese     8     Grill   Fall 2024
    ## 426  28-Oct                     1 Entree + 1 Side   190     Asian   Fall 2024
    ## 427  28-Oct                     1 Entree + 2 Side    86     Asian   Fall 2024
    ## 428  28-Oct                    Bowl Ramen Chicken    65     Asian   Fall 2024
    ## 429  28-Oct                   2 Entrees + 2 Sides    23     Asian   Fall 2024
    ## 430  28-Oct                       Bowl Ramen Tofu    11     Asian   Fall 2024
    ## 431  28-Oct                          1 Wok Entree    10     Asian   Fall 2024
    ## 432  28-Oct               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 433  28-Oct                       Side Vegetables     2     Asian   Fall 2024
    ## 434  28-Oct              Side White or Brown Rice     3     Asian   Fall 2024
    ## 435  28-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 436  28-Oct           Create Your Pasta Bowl MEAT   127   Italian   Fall 2024
    ## 437  28-Oct            Create Your Pasta Bowl VEG    38   Italian   Fall 2024
    ## 438  28-Oct                   Pizza with Toppings    34   Italian   Fall 2024
    ## 439  28-Oct                          Pizza Cheese    22   Italian   Fall 2024
    ## 440  28-Oct                        Add Extra Meat    27   Italian   Fall 2024
    ## 441  28-Oct                     Burrito Breakfast    89 Breakfast   Fall 2024
    ## 442  28-Oct                   Small French Omelet    57 Breakfast   Fall 2024
    ## 443  28-Oct      Egg Cheese Sausage Breakfast San    35 Breakfast   Fall 2024
    ## 444  28-Oct                  Grand Slam Breakfast    17 Breakfast   Fall 2024
    ## 445  28-Oct      Egg Cheese Bacon Breakfast Sandw    28 Breakfast   Fall 2024
    ## 446  28-Oct                             Add Bacon    36 Breakfast   Fall 2024
    ## 447  28-Oct                              Two Eggs    17 Breakfast   Fall 2024
    ## 448  28-Oct                   Trillium Home Fries     6 Breakfast   Fall 2024
    ## 449  28-Oct                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 450  28-Oct                        Pancake Single     1 Breakfast   Fall 2024
    ## 451  28-Oct                                 Toast     1 Breakfast   Fall 2024
    ## 452  28-Oct                              PC Jelly     1 Breakfast   Fall 2024
    ## 453  28-Oct                      Burrito Bowl BYO   107   Mexican   Fall 2024
    ## 454  28-Oct                           Single Taco     2   Mexican   Fall 2024
    ## 455  28-Oct                        Side Guacamole     1   Mexican   Fall 2024
    ## 456  28-Oct           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 457  28-Oct                    Salad by the Pound    63 Salad Bar   Fall 2024
    ## 458  28-Oct                             8 oz Soup    46      Soup   Fall 2024
    ## 459  28-Oct                            Soup 12 oz    40      Soup   Fall 2024
    ## 460  28-Oct                      Side Potato Tots    13 Grab N Go   Fall 2024
    ## 461  29-Oct            Quesadilla Deluxe Trillium   166     Grill   Fall 2024
    ## 462  29-Oct                     Grilled Hamburger   102     Grill   Fall 2024
    ## 463  29-Oct                 Fried Chicken Tenders    89 Grab N Go   Fall 2024
    ## 464  29-Oct         Burrito Una Mano Trillium BYO    63     Grill   Fall 2024
    ## 465  29-Oct                          French Fries   135     Grill   Fall 2024
    ## 466  29-Oct       Grilled Chicken Breast Sandwich    22     Grill   Fall 2024
    ## 467  29-Oct                     Quesadilla Cheese    19     Grill   Fall 2024
    ## 468  29-Oct      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 469  29-Oct                    Sweet Potato Fries    27     Grill   Fall 2024
    ## 470  29-Oct                     Black Bean Burger     6     Grill   Fall 2024
    ## 471  29-Oct                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 472  29-Oct                          + Beef Patty    16     Grill   Fall 2024
    ## 473  29-Oct                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 474  29-Oct                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 475  29-Oct                           Add Egg .99     6     Grill   Fall 2024
    ## 476  29-Oct                            ADD Cheese     9     Grill   Fall 2024
    ## 477  29-Oct                     1 Entree + 1 Side   206     Asian   Fall 2024
    ## 478  29-Oct                     1 Entree + 2 Side    94     Asian   Fall 2024
    ## 479  29-Oct                    Bowl Ramen Chicken    67     Asian   Fall 2024
    ## 480  29-Oct                   2 Entrees + 2 Sides    23     Asian   Fall 2024
    ## 481  29-Oct                       Bowl Ramen Tofu    13     Asian   Fall 2024
    ## 482  29-Oct               Side Vegetarian Lo Mein    12     Asian   Fall 2024
    ## 483  29-Oct                          1 Wok Entree     6     Asian   Fall 2024
    ## 484  29-Oct              Side White or Brown Rice     5     Asian   Fall 2024
    ## 485  29-Oct       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 486  29-Oct                     Burrito Breakfast    90 Breakfast   Fall 2024
    ## 487  29-Oct                   Small French Omelet    60 Breakfast   Fall 2024
    ## 488  29-Oct      Egg Cheese Sausage Breakfast San    34 Breakfast   Fall 2024
    ## 489  29-Oct      Egg Cheese Bacon Breakfast Sandw    32 Breakfast   Fall 2024
    ## 490  29-Oct                  Grand Slam Breakfast    17 Breakfast   Fall 2024
    ## 491  29-Oct                             Add Bacon    29 Breakfast   Fall 2024
    ## 492  29-Oct                              Two Eggs    14 Breakfast   Fall 2024
    ## 493  29-Oct                        Pancake Single     8 Breakfast   Fall 2024
    ## 494  29-Oct                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 495  29-Oct                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 496  29-Oct                                 Toast     1 Breakfast   Fall 2024
    ## 497  29-Oct           Create Your Pasta Bowl MEAT   108   Italian   Fall 2024
    ## 498  29-Oct                   Pizza with Toppings    39   Italian   Fall 2024
    ## 499  29-Oct            Create Your Pasta Bowl VEG    25   Italian   Fall 2024
    ## 500  29-Oct                          Pizza Cheese    22   Italian   Fall 2024
    ## 501  29-Oct                        Add Extra Meat    20   Italian   Fall 2024
    ## 502  29-Oct                      Burrito Bowl BYO   102   Mexican   Fall 2024
    ## 503  29-Oct                           Single Taco     5   Mexican   Fall 2024
    ## 504  29-Oct                            Side Salsa     1   Mexican   Fall 2024
    ## 505  29-Oct           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 506  29-Oct                    Salad by the Pound    67 Salad Bar   Fall 2024
    ## 507  29-Oct                Add Extra Protein 2.99     5 Salad Bar   Fall 2024
    ## 508  29-Oct                            Soup 12 oz    58      Soup   Fall 2024
    ## 509  29-Oct                             8 oz Soup    48      Soup   Fall 2024
    ## 510  29-Oct                      Side Potato Tots    18 Grab N Go   Fall 2024
    ## 511  30-Oct            Quesadilla Deluxe Trillium   167     Grill   Fall 2024
    ## 512  30-Oct                     Grilled Hamburger    95     Grill   Fall 2024
    ## 513  30-Oct                 Fried Chicken Tenders    95 Grab N Go   Fall 2024
    ## 514  30-Oct         Burrito Una Mano Trillium BYO    64     Grill   Fall 2024
    ## 515  30-Oct                          French Fries   110     Grill   Fall 2024
    ## 516  30-Oct       Grilled Chicken Breast Sandwich    19     Grill   Fall 2024
    ## 517  30-Oct      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 518  30-Oct                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 519  30-Oct                     Quesadilla Cheese     7     Grill   Fall 2024
    ## 520  30-Oct                     Black Bean Burger     5     Grill   Fall 2024
    ## 521  30-Oct                    Sweet Potato Fries    12     Grill   Fall 2024
    ## 522  30-Oct                          + Beef Patty     4     Grill   Fall 2024
    ## 523  30-Oct                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 524  30-Oct                           Add Egg .99     5     Grill   Fall 2024
    ## 525  30-Oct                            ADD Cheese     7     Grill   Fall 2024
    ## 526  30-Oct                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 527  30-Oct                     1 Entree + 1 Side   175     Asian   Fall 2024
    ## 528  30-Oct                     1 Entree + 2 Side    83     Asian   Fall 2024
    ## 529  30-Oct                    Bowl Ramen Chicken    70     Asian   Fall 2024
    ## 530  30-Oct                   2 Entrees + 2 Sides    23     Asian   Fall 2024
    ## 531  30-Oct                       Bowl Ramen Tofu    12     Asian   Fall 2024
    ## 532  30-Oct               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 533  30-Oct                          1 Wok Entree     3     Asian   Fall 2024
    ## 534  30-Oct              Side White or Brown Rice     5     Asian   Fall 2024
    ## 535  30-Oct                       Side Vegetables     1     Asian   Fall 2024
    ## 536  30-Oct           Create Your Pasta Bowl MEAT   136   Italian   Fall 2024
    ## 537  30-Oct            Create Your Pasta Bowl VEG    23   Italian   Fall 2024
    ## 538  30-Oct                   Pizza with Toppings    31   Italian   Fall 2024
    ## 539  30-Oct                          Pizza Cheese    17   Italian   Fall 2024
    ## 540  30-Oct                        Add Extra Meat    28   Italian   Fall 2024
    ## 541  30-Oct              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 542  30-Oct                     Burrito Breakfast    80 Breakfast   Fall 2024
    ## 543  30-Oct                   Small French Omelet    57 Breakfast   Fall 2024
    ## 544  30-Oct                  Grand Slam Breakfast    21 Breakfast   Fall 2024
    ## 545  30-Oct      Egg Cheese Bacon Breakfast Sandw    37 Breakfast   Fall 2024
    ## 546  30-Oct      Egg Cheese Sausage Breakfast San    31 Breakfast   Fall 2024
    ## 547  30-Oct                             Add Bacon    32 Breakfast   Fall 2024
    ## 548  30-Oct                              Two Eggs    10 Breakfast   Fall 2024
    ## 549  30-Oct                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 550  30-Oct                        Pancake Single     2 Breakfast   Fall 2024
    ## 551  30-Oct                      PC Peanut Butter     2 Breakfast   Fall 2024
    ## 552  30-Oct                                 Toast     1 Breakfast   Fall 2024
    ## 553  30-Oct                      Burrito Bowl BYO    96   Mexican   Fall 2024
    ## 554  30-Oct                           Single Taco     9   Mexican   Fall 2024
    ## 555  30-Oct                        Side Guacamole     3   Mexican   Fall 2024
    ## 556  30-Oct                            Side Salsa     1   Mexican   Fall 2024
    ## 557  30-Oct                    Salad by the Pound    76 Salad Bar   Fall 2024
    ## 558  30-Oct                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 559  30-Oct                             8 oz Soup    63      Soup   Fall 2024
    ## 560  30-Oct                            Soup 12 oz    32      Soup   Fall 2024
    ## 561  30-Oct                      Side Potato Tots    20 Grab N Go   Fall 2024
    ## 562  31-Oct            Quesadilla Deluxe Trillium   164     Grill   Fall 2024
    ## 563  31-Oct                     Grilled Hamburger   107     Grill   Fall 2024
    ## 564  31-Oct                 Fried Chicken Tenders   100 Grab N Go   Fall 2024
    ## 565  31-Oct         Burrito Una Mano Trillium BYO    70     Grill   Fall 2024
    ## 566  31-Oct                          French Fries   135     Grill   Fall 2024
    ## 567  31-Oct       Grilled Chicken Breast Sandwich    21     Grill   Fall 2024
    ## 568  31-Oct      Trillium Grill Impossible Burger    13     Grill   Fall 2024
    ## 569  31-Oct                     Quesadilla Cheese    14     Grill   Fall 2024
    ## 570  31-Oct                    Sweet Potato Fries    27     Grill   Fall 2024
    ## 571  31-Oct                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 572  31-Oct                          + Beef Patty    14     Grill   Fall 2024
    ## 573  31-Oct                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 574  31-Oct                     Black Bean Burger     1     Grill   Fall 2024
    ## 575  31-Oct                            ADD Cheese    12     Grill   Fall 2024
    ## 576  31-Oct                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 577  31-Oct                           Add Egg .99     2     Grill   Fall 2024
    ## 578  31-Oct                     1 Entree + 1 Side   226     Asian   Fall 2024
    ## 579  31-Oct                     1 Entree + 2 Side    77     Asian   Fall 2024
    ## 580  31-Oct                    Bowl Ramen Chicken    79     Asian   Fall 2024
    ## 581  31-Oct                   2 Entrees + 2 Sides    29     Asian   Fall 2024
    ## 582  31-Oct                       Bowl Ramen Tofu    10     Asian   Fall 2024
    ## 583  31-Oct               Side Vegetarian Lo Mein    10     Asian   Fall 2024
    ## 584  31-Oct                          1 Wok Entree     6     Asian   Fall 2024
    ## 585  31-Oct       Side Vegetarian Fried Rice with     4     Asian   Fall 2024
    ## 586  31-Oct              Side White or Brown Rice     6     Asian   Fall 2024
    ## 587  31-Oct           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 588  31-Oct                       Side Vegetables     1     Asian   Fall 2024
    ## 589  31-Oct                     Burrito Breakfast    92 Breakfast   Fall 2024
    ## 590  31-Oct                   Small French Omelet    55 Breakfast   Fall 2024
    ## 591  31-Oct      Egg Cheese Sausage Breakfast San    34 Breakfast   Fall 2024
    ## 592  31-Oct      Egg Cheese Bacon Breakfast Sandw    30 Breakfast   Fall 2024
    ## 593  31-Oct                  Grand Slam Breakfast    16 Breakfast   Fall 2024
    ## 594  31-Oct                             Add Bacon    23 Breakfast   Fall 2024
    ## 595  31-Oct                              Two Eggs    19 Breakfast   Fall 2024
    ## 596  31-Oct                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 597  31-Oct                        Pancake Single     1 Breakfast   Fall 2024
    ## 598  31-Oct                                 Toast     2 Breakfast   Fall 2024
    ## 599  31-Oct                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 600  31-Oct           Create Your Pasta Bowl MEAT   120   Italian   Fall 2024
    ## 601  31-Oct            Create Your Pasta Bowl VEG    27   Italian   Fall 2024
    ## 602  31-Oct                   Pizza with Toppings    23   Italian   Fall 2024
    ## 603  31-Oct                          Pizza Cheese    25   Italian   Fall 2024
    ## 604  31-Oct                        Add Extra Meat    16   Italian   Fall 2024
    ## 605  31-Oct              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 606  31-Oct                      Burrito Bowl BYO    93   Mexican   Fall 2024
    ## 607  31-Oct                           Single Taco     2   Mexican   Fall 2024
    ## 608  31-Oct                        Side Guacamole     2   Mexican   Fall 2024
    ## 609  31-Oct                            Side Salsa     1   Mexican   Fall 2024
    ## 610  31-Oct                    Salad by the Pound    62 Salad Bar   Fall 2024
    ## 611  31-Oct                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 612  31-Oct                             8 oz Soup    27      Soup   Fall 2024
    ## 613  31-Oct                            Soup 12 oz    21      Soup   Fall 2024
    ## 614  31-Oct                      Side Potato Tots    22 Grab N Go   Fall 2024
    ## 615   1-Nov            Quesadilla Deluxe Trillium   118     Grill   Fall 2024
    ## 616   1-Nov                     Grilled Hamburger    66     Grill   Fall 2024
    ## 617   1-Nov                 Fried Chicken Tenders    71 Grab N Go   Fall 2024
    ## 618   1-Nov         Burrito Una Mano Trillium BYO    48     Grill   Fall 2024
    ## 619   1-Nov                          French Fries    95     Grill   Fall 2024
    ## 620   1-Nov                     Quesadilla Cheese    15     Grill   Fall 2024
    ## 621   1-Nov       Grilled Chicken Breast Sandwich    12     Grill   Fall 2024
    ## 622   1-Nov      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 623   1-Nov                  Seared Salmon Burger     4     Grill   Fall 2024
    ## 624   1-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 625   1-Nov                          + Beef Patty     8     Grill   Fall 2024
    ## 626   1-Nov                    Sweet Potato Fries     4     Grill   Fall 2024
    ## 627   1-Nov                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 628   1-Nov                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 629   1-Nov                           Add Egg .99     1     Grill   Fall 2024
    ## 630   1-Nov                     1 Entree + 1 Side   117     Asian   Fall 2024
    ## 631   1-Nov                     1 Entree + 2 Side    51     Asian   Fall 2024
    ## 632   1-Nov                    Bowl Ramen Chicken    34     Asian   Fall 2024
    ## 633   1-Nov                   2 Entrees + 2 Sides    12     Asian   Fall 2024
    ## 634   1-Nov                       Bowl Ramen Tofu    13     Asian   Fall 2024
    ## 635   1-Nov               Side Vegetarian Lo Mein     8     Asian   Fall 2024
    ## 636   1-Nov                          1 Wok Entree     4     Asian   Fall 2024
    ## 637   1-Nov                       Side Vegetables     3     Asian   Fall 2024
    ## 638   1-Nov              Side White or Brown Rice     5     Asian   Fall 2024
    ## 639   1-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 640   1-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 641   1-Nov                     Burrito Breakfast    57 Breakfast   Fall 2024
    ## 642   1-Nov                  Grand Slam Breakfast    28 Breakfast   Fall 2024
    ## 643   1-Nov                   Small French Omelet    25 Breakfast   Fall 2024
    ## 644   1-Nov      Egg Cheese Bacon Breakfast Sandw    24 Breakfast   Fall 2024
    ## 645   1-Nov      Egg Cheese Sausage Breakfast San    21 Breakfast   Fall 2024
    ## 646   1-Nov                             Add Bacon    18 Breakfast   Fall 2024
    ## 647   1-Nov                              Two Eggs    10 Breakfast   Fall 2024
    ## 648   1-Nov                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 649   1-Nov                        Pancake Single     4 Breakfast   Fall 2024
    ## 650   1-Nov           Create Your Pasta Bowl MEAT    87   Italian   Fall 2024
    ## 651   1-Nov                   Pizza with Toppings    17   Italian   Fall 2024
    ## 652   1-Nov            Create Your Pasta Bowl VEG     8   Italian   Fall 2024
    ## 653   1-Nov                          Pizza Cheese    13   Italian   Fall 2024
    ## 654   1-Nov                        Add Extra Meat     7   Italian   Fall 2024
    ## 655   1-Nov                      Burrito Bowl BYO    59   Mexican   Fall 2024
    ## 656   1-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 657   1-Nov                        Side Guacamole     2   Mexican   Fall 2024
    ## 658   1-Nov                    Salad by the Pound    45 Salad Bar   Fall 2024
    ## 659   1-Nov                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 660   1-Nov                             8 oz Soup    40      Soup   Fall 2024
    ## 661   1-Nov                            Soup 12 oz    21      Soup   Fall 2024
    ## 662   1-Nov                      Side Potato Tots    12 Grab N Go   Fall 2024
    ## 663   4-Nov            Quesadilla Deluxe Trillium   170     Grill   Fall 2024
    ## 664   4-Nov                     Grilled Hamburger    80     Grill   Fall 2024
    ## 665   4-Nov                 Fried Chicken Tenders    95 Grab N Go   Fall 2024
    ## 666   4-Nov         Burrito Una Mano Trillium BYO    71     Grill   Fall 2024
    ## 667   4-Nov                          French Fries   176     Grill   Fall 2024
    ## 668   4-Nov                     Quesadilla Cheese    15     Grill   Fall 2024
    ## 669   4-Nov       Grilled Chicken Breast Sandwich    12     Grill   Fall 2024
    ## 670   4-Nov                  Seared Salmon Burger    11     Grill   Fall 2024
    ## 671   4-Nov      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 672   4-Nov                          + Beef Patty    13     Grill   Fall 2024
    ## 673   4-Nov                     Black Bean Burger     4     Grill   Fall 2024
    ## 674   4-Nov                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 675   4-Nov                            ADD Cheese     5     Grill   Fall 2024
    ## 676   4-Nov                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 677   4-Nov                           Add Egg .99     1     Grill   Fall 2024
    ## 678   4-Nov                     1 Entree + 1 Side   171     Asian   Fall 2024
    ## 679   4-Nov                     1 Entree + 2 Side    84     Asian   Fall 2024
    ## 680   4-Nov                    Bowl Ramen Chicken    65     Asian   Fall 2024
    ## 681   4-Nov                   2 Entrees + 2 Sides    26     Asian   Fall 2024
    ## 682   4-Nov                       Bowl Ramen Tofu    20     Asian   Fall 2024
    ## 683   4-Nov                          1 Wok Entree     7     Asian   Fall 2024
    ## 684   4-Nov               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 685   4-Nov       Side Vegetarian Fried Rice with     5     Asian   Fall 2024
    ## 686   4-Nov                       Side Vegetables     3     Asian   Fall 2024
    ## 687   4-Nov              Side White or Brown Rice     1     Asian   Fall 2024
    ## 688   4-Nov                     Burrito Breakfast    97 Breakfast   Fall 2024
    ## 689   4-Nov                   Small French Omelet    51 Breakfast   Fall 2024
    ## 690   4-Nov      Egg Cheese Sausage Breakfast San    45 Breakfast   Fall 2024
    ## 691   4-Nov      Egg Cheese Bacon Breakfast Sandw    42 Breakfast   Fall 2024
    ## 692   4-Nov                  Grand Slam Breakfast    17 Breakfast   Fall 2024
    ## 693   4-Nov                             Add Bacon    28 Breakfast   Fall 2024
    ## 694   4-Nov                              Two Eggs    10 Breakfast   Fall 2024
    ## 695   4-Nov                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 696   4-Nov                        Pancake Single     3 Breakfast   Fall 2024
    ## 697   4-Nov                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 698   4-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 699   4-Nov           Create Your Pasta Bowl MEAT   118   Italian   Fall 2024
    ## 700   4-Nov            Create Your Pasta Bowl VEG    34   Italian   Fall 2024
    ## 701   4-Nov                   Pizza with Toppings    27   Italian   Fall 2024
    ## 702   4-Nov                          Pizza Cheese    17   Italian   Fall 2024
    ## 703   4-Nov                        Add Extra Meat    27   Italian   Fall 2024
    ## 704   4-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 705   4-Nov                      Burrito Bowl BYO    91   Mexican   Fall 2024
    ## 706   4-Nov                           Single Taco    11   Mexican   Fall 2024
    ## 707   4-Nov                        Side Guacamole     2   Mexican   Fall 2024
    ## 708   4-Nov           Add Extra Toppings Una Mano     4   Mexican   Fall 2024
    ## 709   4-Nov                    Salad by the Pound    46 Salad Bar   Fall 2024
    ## 710   4-Nov                            Soup 12 oz    44      Soup   Fall 2024
    ## 711   4-Nov                             8 oz Soup    32      Soup   Fall 2024
    ## 712   4-Nov                      Side Potato Tots     9 Grab N Go   Fall 2024
    ## 713   5-Nov            Quesadilla Deluxe Trillium   165     Grill   Fall 2024
    ## 714   5-Nov                     Grilled Hamburger   107     Grill   Fall 2024
    ## 715   5-Nov                 Fried Chicken Tenders   105 Grab N Go   Fall 2024
    ## 716   5-Nov         Burrito Una Mano Trillium BYO    63     Grill   Fall 2024
    ## 717   5-Nov                          French Fries   136     Grill   Fall 2024
    ## 718   5-Nov      Trillium Grill Impossible Burger    14     Grill   Fall 2024
    ## 719   5-Nov       Grilled Chicken Breast Sandwich    11     Grill   Fall 2024
    ## 720   5-Nov                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 721   5-Nov                  Seared Salmon Burger     4     Grill   Fall 2024
    ## 722   5-Nov                    Sweet Potato Fries    11     Grill   Fall 2024
    ## 723   5-Nov                          + Beef Patty     9     Grill   Fall 2024
    ## 724   5-Nov                     Black Bean Burger     2     Grill   Fall 2024
    ## 725   5-Nov                    ADD Chicken Breast     4     Grill   Fall 2024
    ## 726   5-Nov                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 727   5-Nov                            ADD Cheese     2     Grill   Fall 2024
    ## 728   5-Nov                           Add Egg .99     1     Grill   Fall 2024
    ## 729   5-Nov                     1 Entree + 1 Side   212     Asian   Fall 2024
    ## 730   5-Nov                     1 Entree + 2 Side    79     Asian   Fall 2024
    ## 731   5-Nov                    Bowl Ramen Chicken    73     Asian   Fall 2024
    ## 732   5-Nov                   2 Entrees + 2 Sides    27     Asian   Fall 2024
    ## 733   5-Nov                       Bowl Ramen Tofu    11     Asian   Fall 2024
    ## 734   5-Nov               Side Vegetarian Lo Mein    10     Asian   Fall 2024
    ## 735   5-Nov                          1 Wok Entree     5     Asian   Fall 2024
    ## 736   5-Nov                       Side Vegetables     1     Asian   Fall 2024
    ## 737   5-Nov                     Burrito Breakfast    83 Breakfast   Fall 2024
    ## 738   5-Nov                   Small French Omelet    52 Breakfast   Fall 2024
    ## 739   5-Nov      Egg Cheese Bacon Breakfast Sandw    37 Breakfast   Fall 2024
    ## 740   5-Nov      Egg Cheese Sausage Breakfast San    33 Breakfast   Fall 2024
    ## 741   5-Nov                  Grand Slam Breakfast    15 Breakfast   Fall 2024
    ## 742   5-Nov                             Add Bacon    29 Breakfast   Fall 2024
    ## 743   5-Nov                              Two Eggs    18 Breakfast   Fall 2024
    ## 744   5-Nov                        Pancake Single     5 Breakfast   Fall 2024
    ## 745   5-Nov                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 746   5-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 747   5-Nov           Create Your Pasta Bowl MEAT   103   Italian   Fall 2024
    ## 748   5-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 749   5-Nov            Create Your Pasta Bowl VEG    20   Italian   Fall 2024
    ## 750   5-Nov                          Pizza Cheese    20   Italian   Fall 2024
    ## 751   5-Nov                        Add Extra Meat     6   Italian   Fall 2024
    ## 752   5-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 753   5-Nov                      Burrito Bowl BYO   109   Mexican   Fall 2024
    ## 754   5-Nov                           Single Taco     4   Mexican   Fall 2024
    ## 755   5-Nov           Add Extra Toppings Una Mano     6   Mexican   Fall 2024
    ## 756   5-Nov                        Side Guacamole     1   Mexican   Fall 2024
    ## 757   5-Nov                            Side Salsa     1   Mexican   Fall 2024
    ## 758   5-Nov                    Salad by the Pound    55 Salad Bar   Fall 2024
    ## 759   5-Nov                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 760   5-Nov                            Soup 12 oz    37      Soup   Fall 2024
    ## 761   5-Nov                             8 oz Soup    30      Soup   Fall 2024
    ## 762   5-Nov                      Side Potato Tots    22 Grab N Go   Fall 2024
    ## 763   6-Nov            Quesadilla Deluxe Trillium   130     Grill   Fall 2024
    ## 764   6-Nov                     Grilled Hamburger   104     Grill   Fall 2024
    ## 765   6-Nov                 Fried Chicken Tenders    94 Grab N Go   Fall 2024
    ## 766   6-Nov         Burrito Una Mano Trillium BYO    59     Grill   Fall 2024
    ## 767   6-Nov                          French Fries   148     Grill   Fall 2024
    ## 768   6-Nov       Grilled Chicken Breast Sandwich    16     Grill   Fall 2024
    ## 769   6-Nov                    Sweet Potato Fries    37     Grill   Fall 2024
    ## 770   6-Nov      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 771   6-Nov                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 772   6-Nov                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 773   6-Nov                    ADD Chicken Breast     8     Grill   Fall 2024
    ## 774   6-Nov                          + Beef Patty     7     Grill   Fall 2024
    ## 775   6-Nov                     Black Bean Burger     2     Grill   Fall 2024
    ## 776   6-Nov                   Add Sausage 2 Patty     6     Grill   Fall 2024
    ## 777   6-Nov                            ADD Cheese     7     Grill   Fall 2024
    ## 778   6-Nov                           Add Egg .99     3     Grill   Fall 2024
    ## 779   6-Nov                     1 Entree + 1 Side   209     Asian   Fall 2024
    ## 780   6-Nov                    Bowl Ramen Chicken    90     Asian   Fall 2024
    ## 781   6-Nov                     1 Entree + 2 Side    54     Asian   Fall 2024
    ## 782   6-Nov                   2 Entrees + 2 Sides    27     Asian   Fall 2024
    ## 783   6-Nov                       Bowl Ramen Tofu    17     Asian   Fall 2024
    ## 784   6-Nov                          1 Wok Entree    14     Asian   Fall 2024
    ## 785   6-Nov               Side Vegetarian Lo Mein     9     Asian   Fall 2024
    ## 786   6-Nov       Side Vegetarian Fried Rice with     7     Asian   Fall 2024
    ## 787   6-Nov              Side White or Brown Rice    13     Asian   Fall 2024
    ## 788   6-Nov                       Side Vegetables     2     Asian   Fall 2024
    ## 789   6-Nov           Create Your Pasta Bowl MEAT   136   Italian   Fall 2024
    ## 790   6-Nov            Create Your Pasta Bowl VEG    30   Italian   Fall 2024
    ## 791   6-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 792   6-Nov                          Pizza Cheese    20   Italian   Fall 2024
    ## 793   6-Nov                        Add Extra Meat    27   Italian   Fall 2024
    ## 794   6-Nov                     Burrito Breakfast    94 Breakfast   Fall 2024
    ## 795   6-Nov                   Small French Omelet    64 Breakfast   Fall 2024
    ## 796   6-Nov                  Grand Slam Breakfast    17 Breakfast   Fall 2024
    ## 797   6-Nov      Egg Cheese Bacon Breakfast Sandw    24 Breakfast   Fall 2024
    ## 798   6-Nov      Egg Cheese Sausage Breakfast San    23 Breakfast   Fall 2024
    ## 799   6-Nov                             Add Bacon    27 Breakfast   Fall 2024
    ## 800   6-Nov                              Two Eggs    19 Breakfast   Fall 2024
    ## 801   6-Nov                   Trillium Home Fries     7 Breakfast   Fall 2024
    ## 802   6-Nov                        Pancake Single     6 Breakfast   Fall 2024
    ## 803   6-Nov                                 Toast     3 Breakfast   Fall 2024
    ## 804   6-Nov                      PC Peanut Butter     2 Breakfast   Fall 2024
    ## 805   6-Nov                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 806   6-Nov                      Burrito Bowl BYO   109   Mexican   Fall 2024
    ## 807   6-Nov                           Single Taco     5   Mexican   Fall 2024
    ## 808   6-Nov                       Side Sour Cream     1   Mexican   Fall 2024
    ## 809   6-Nov                    Salad by the Pound    59 Salad Bar   Fall 2024
    ## 810   6-Nov                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 811   6-Nov                             8 oz Soup    54      Soup   Fall 2024
    ## 812   6-Nov                            Soup 12 oz    40      Soup   Fall 2024
    ## 813   6-Nov                      Side Potato Tots    19 Grab N Go   Fall 2024
    ## 814   7-Nov            Quesadilla Deluxe Trillium   160     Grill   Fall 2024
    ## 815   7-Nov                     Grilled Hamburger    98     Grill   Fall 2024
    ## 816   7-Nov                 Fried Chicken Tenders   105 Grab N Go   Fall 2024
    ## 817   7-Nov         Burrito Una Mano Trillium BYO    64     Grill   Fall 2024
    ## 818   7-Nov                          French Fries   141     Grill   Fall 2024
    ## 819   7-Nov       Grilled Chicken Breast Sandwich    19     Grill   Fall 2024
    ## 820   7-Nov                  Seared Salmon Burger    14     Grill   Fall 2024
    ## 821   7-Nov                    Sweet Potato Fries    33     Grill   Fall 2024
    ## 822   7-Nov      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 823   7-Nov                     Quesadilla Cheese     9     Grill   Fall 2024
    ## 824   7-Nov                          + Beef Patty    12     Grill   Fall 2024
    ## 825   7-Nov                     Black Bean Burger     4     Grill   Fall 2024
    ## 826   7-Nov                    ADD Chicken Breast     9     Grill   Fall 2024
    ## 827   7-Nov                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 828   7-Nov                           Add Egg .99     3     Grill   Fall 2024
    ## 829   7-Nov                            ADD Cheese     3     Grill   Fall 2024
    ## 830   7-Nov                     1 Entree + 1 Side   193     Asian   Fall 2024
    ## 831   7-Nov                     1 Entree + 2 Side    94     Asian   Fall 2024
    ## 832   7-Nov                    Bowl Ramen Chicken    82     Asian   Fall 2024
    ## 833   7-Nov                   2 Entrees + 2 Sides    30     Asian   Fall 2024
    ## 834   7-Nov                       Bowl Ramen Tofu    14     Asian   Fall 2024
    ## 835   7-Nov               Side Vegetarian Lo Mein    12     Asian   Fall 2024
    ## 836   7-Nov                          1 Wok Entree     5     Asian   Fall 2024
    ## 837   7-Nov              Side White or Brown Rice     5     Asian   Fall 2024
    ## 838   7-Nov                    Bowl Ramen Chicken     1     Asian   Fall 2024
    ## 839   7-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 840   7-Nov           Create Your Pasta Bowl MEAT   132   Italian   Fall 2024
    ## 841   7-Nov            Create Your Pasta Bowl VEG    28   Italian   Fall 2024
    ## 842   7-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 843   7-Nov                          Pizza Cheese    21   Italian   Fall 2024
    ## 844   7-Nov                        Add Extra Meat    18   Italian   Fall 2024
    ## 845   7-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 846   7-Nov                     Burrito Breakfast    90 Breakfast   Fall 2024
    ## 847   7-Nov                   Small French Omelet    60 Breakfast   Fall 2024
    ## 848   7-Nov                  Grand Slam Breakfast    17 Breakfast   Fall 2024
    ## 849   7-Nov      Egg Cheese Sausage Breakfast San    24 Breakfast   Fall 2024
    ## 850   7-Nov      Egg Cheese Bacon Breakfast Sandw    22 Breakfast   Fall 2024
    ## 851   7-Nov                             Add Bacon    24 Breakfast   Fall 2024
    ## 852   7-Nov                              Two Eggs    19 Breakfast   Fall 2024
    ## 853   7-Nov                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 854   7-Nov                        Pancake Single     2 Breakfast   Fall 2024
    ## 855   7-Nov                      PC Peanut Butter     2 Breakfast   Fall 2024
    ## 856   7-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 857   7-Nov                      Burrito Bowl BYO    98   Mexican   Fall 2024
    ## 858   7-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 859   7-Nov                        Side Guacamole     5   Mexican   Fall 2024
    ## 860   7-Nov           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 861   7-Nov                            Side Salsa     1   Mexican   Fall 2024
    ## 862   7-Nov                    Salad by the Pound    69 Salad Bar   Fall 2024
    ## 863   7-Nov                             8 oz Soup    41      Soup   Fall 2024
    ## 864   7-Nov                            Soup 12 oz    34      Soup   Fall 2024
    ## 865   7-Nov                      Side Potato Tots    25 Grab N Go   Fall 2024
    ## 866   8-Nov            Quesadilla Deluxe Trillium   116     Grill   Fall 2024
    ## 867   8-Nov                     Grilled Hamburger    80     Grill   Fall 2024
    ## 868   8-Nov         Burrito Una Mano Trillium BYO    45     Grill   Fall 2024
    ## 869   8-Nov                 Fried Chicken Tenders    55 Grab N Go   Fall 2024
    ## 870   8-Nov                          French Fries    97     Grill   Fall 2024
    ## 871   8-Nov       Grilled Chicken Breast Sandwich    12     Grill   Fall 2024
    ## 872   8-Nov                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 873   8-Nov                    Sweet Potato Fries    32     Grill   Fall 2024
    ## 874   8-Nov      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 875   8-Nov                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 876   8-Nov                          + Beef Patty     5     Grill   Fall 2024
    ## 877   8-Nov                   Add Sausage 2 Patty     4     Grill   Fall 2024
    ## 878   8-Nov                     Black Bean Burger     1     Grill   Fall 2024
    ## 879   8-Nov                    ADD Chicken Breast     2     Grill   Fall 2024
    ## 880   8-Nov                            ADD Cheese     6     Grill   Fall 2024
    ## 881   8-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 882   8-Nov                     1 Entree + 1 Side   126     Asian   Fall 2024
    ## 883   8-Nov                    Bowl Ramen Chicken    59     Asian   Fall 2024
    ## 884   8-Nov                     1 Entree + 2 Side    53     Asian   Fall 2024
    ## 885   8-Nov                   2 Entrees + 2 Sides    20     Asian   Fall 2024
    ## 886   8-Nov                       Bowl Ramen Tofu    14     Asian   Fall 2024
    ## 887   8-Nov               Side Vegetarian Lo Mein     8     Asian   Fall 2024
    ## 888   8-Nov                          1 Wok Entree     2     Asian   Fall 2024
    ## 889   8-Nov       Side Vegetarian Fried Rice with     3     Asian   Fall 2024
    ## 890   8-Nov                     Burrito Breakfast    97 Breakfast   Fall 2024
    ## 891   8-Nov                   Small French Omelet    57 Breakfast   Fall 2024
    ## 892   8-Nov                  Grand Slam Breakfast    18 Breakfast   Fall 2024
    ## 893   8-Nov      Egg Cheese Bacon Breakfast Sandw    25 Breakfast   Fall 2024
    ## 894   8-Nov      Egg Cheese Sausage Breakfast San    23 Breakfast   Fall 2024
    ## 895   8-Nov                              Two Eggs    24 Breakfast   Fall 2024
    ## 896   8-Nov                             Add Bacon    23 Breakfast   Fall 2024
    ## 897   8-Nov                        Pancake Single     5 Breakfast   Fall 2024
    ## 898   8-Nov                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 899   8-Nov                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 900   8-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 901   8-Nov           Create Your Pasta Bowl MEAT    92   Italian   Fall 2024
    ## 902   8-Nov            Create Your Pasta Bowl VEG    16   Italian   Fall 2024
    ## 903   8-Nov                   Pizza with Toppings    22   Italian   Fall 2024
    ## 904   8-Nov                          Pizza Cheese    12   Italian   Fall 2024
    ## 905   8-Nov                        Add Extra Meat    12   Italian   Fall 2024
    ## 906   8-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 907   8-Nov                      Burrito Bowl BYO    71   Mexican   Fall 2024
    ## 908   8-Nov                        Side Guacamole     3   Mexican   Fall 2024
    ## 909   8-Nov                           Single Taco     1   Mexican   Fall 2024
    ## 910   8-Nov                    Salad by the Pound    41 Salad Bar   Fall 2024
    ## 911   8-Nov                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 912   8-Nov                            Soup 12 oz    33      Soup   Fall 2024
    ## 913   8-Nov                             8 oz Soup    29      Soup   Fall 2024
    ## 914   8-Nov                      Side Potato Tots    20 Grab N Go   Fall 2024
    ## 915  11-Nov            Quesadilla Deluxe Trillium   162     Grill   Fall 2024
    ## 916  11-Nov                     Grilled Hamburger    81     Grill   Fall 2024
    ## 917  11-Nov                 Fried Chicken Tenders    86 Grab N Go   Fall 2024
    ## 918  11-Nov         Burrito Una Mano Trillium BYO    51     Grill   Fall 2024
    ## 919  11-Nov                          French Fries   104     Grill   Fall 2024
    ## 920  11-Nov       Grilled Chicken Breast Sandwich    16     Grill   Fall 2024
    ## 921  11-Nov                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 922  11-Nov                    Sweet Potato Fries    32     Grill   Fall 2024
    ## 923  11-Nov      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 924  11-Nov                  Seared Salmon Burger     4     Grill   Fall 2024
    ## 925  11-Nov                    ADD Chicken Breast     8     Grill   Fall 2024
    ## 926  11-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 927  11-Nov                          + Beef Patty     4     Grill   Fall 2024
    ## 928  11-Nov                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 929  11-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 930  11-Nov                            ADD Cheese     3     Grill   Fall 2024
    ## 931  11-Nov                     1 Entree + 1 Side   171     Asian   Fall 2024
    ## 932  11-Nov                     1 Entree + 2 Side    79     Asian   Fall 2024
    ## 933  11-Nov                    Bowl Ramen Chicken    69     Asian   Fall 2024
    ## 934  11-Nov                   2 Entrees + 2 Sides    24     Asian   Fall 2024
    ## 935  11-Nov                       Bowl Ramen Tofu    12     Asian   Fall 2024
    ## 936  11-Nov               Side Vegetarian Lo Mein    13     Asian   Fall 2024
    ## 937  11-Nov                          1 Wok Entree     2     Asian   Fall 2024
    ## 938  11-Nov              Side White or Brown Rice     2     Asian   Fall 2024
    ## 939  11-Nov                       Side Vegetables     1     Asian   Fall 2024
    ## 940  11-Nov           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 941  11-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 942  11-Nov           Create Your Pasta Bowl MEAT   119   Italian   Fall 2024
    ## 943  11-Nov            Create Your Pasta Bowl VEG    25   Italian   Fall 2024
    ## 944  11-Nov                   Pizza with Toppings    34   Italian   Fall 2024
    ## 945  11-Nov                          Pizza Cheese    16   Italian   Fall 2024
    ## 946  11-Nov                        Add Extra Meat    26   Italian   Fall 2024
    ## 947  11-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 948  11-Nov                     Burrito Breakfast    91 Breakfast   Fall 2024
    ## 949  11-Nov                   Small French Omelet    54 Breakfast   Fall 2024
    ## 950  11-Nov                  Grand Slam Breakfast    15 Breakfast   Fall 2024
    ## 951  11-Nov      Egg Cheese Sausage Breakfast San    20 Breakfast   Fall 2024
    ## 952  11-Nov      Egg Cheese Bacon Breakfast Sandw    19 Breakfast   Fall 2024
    ## 953  11-Nov                             Add Bacon    28 Breakfast   Fall 2024
    ## 954  11-Nov                              Two Eggs    18 Breakfast   Fall 2024
    ## 955  11-Nov                   Trillium Home Fries     7 Breakfast   Fall 2024
    ## 956  11-Nov                        Pancake Single     3 Breakfast   Fall 2024
    ## 957  11-Nov                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 958  11-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 959  11-Nov                      Burrito Bowl BYO   107   Mexican   Fall 2024
    ## 960  11-Nov                           Single Taco     3   Mexican   Fall 2024
    ## 961  11-Nov                    Salad by the Pound    60 Salad Bar   Fall 2024
    ## 962  11-Nov                            Soup 12 oz    49      Soup   Fall 2024
    ## 963  11-Nov                             8 oz Soup    48      Soup   Fall 2024
    ## 964  11-Nov                      Side Potato Tots    22 Grab N Go   Fall 2024
    ## 965  12-Nov            Quesadilla Deluxe Trillium   166     Grill   Fall 2024
    ## 966  12-Nov                     Grilled Hamburger    89     Grill   Fall 2024
    ## 967  12-Nov                 Fried Chicken Tenders    96 Grab N Go   Fall 2024
    ## 968  12-Nov         Burrito Una Mano Trillium BYO    61     Grill   Fall 2024
    ## 969  12-Nov                          French Fries   162     Grill   Fall 2024
    ## 970  12-Nov       Grilled Chicken Breast Sandwich    27     Grill   Fall 2024
    ## 971  12-Nov                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 972  12-Nov      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 973  12-Nov                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 974  12-Nov                          + Beef Patty    17     Grill   Fall 2024
    ## 975  12-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 976  12-Nov                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 977  12-Nov                            ADD Cheese    12     Grill   Fall 2024
    ## 978  12-Nov                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 979  12-Nov                           Add Egg .99     4     Grill   Fall 2024
    ## 980  12-Nov                     1 Entree + 1 Side   186     Asian   Fall 2024
    ## 981  12-Nov                     1 Entree + 2 Side    85     Asian   Fall 2024
    ## 982  12-Nov                    Bowl Ramen Chicken    75     Asian   Fall 2024
    ## 983  12-Nov                   2 Entrees + 2 Sides    32     Asian   Fall 2024
    ## 984  12-Nov                       Bowl Ramen Tofu    19     Asian   Fall 2024
    ## 985  12-Nov                          1 Wok Entree    10     Asian   Fall 2024
    ## 986  12-Nov               Side Vegetarian Lo Mein    15     Asian   Fall 2024
    ## 987  12-Nov              Side White or Brown Rice    14     Asian   Fall 2024
    ## 988  12-Nov                    Bowl Ramen Chicken     1     Asian   Fall 2024
    ## 989  12-Nov                       Side Vegetables     2     Asian   Fall 2024
    ## 990  12-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 991  12-Nov                     Burrito Breakfast    84 Breakfast   Fall 2024
    ## 992  12-Nov                   Small French Omelet    55 Breakfast   Fall 2024
    ## 993  12-Nov                  Grand Slam Breakfast    23 Breakfast   Fall 2024
    ## 994  12-Nov      Egg Cheese Sausage Breakfast San    36 Breakfast   Fall 2024
    ## 995  12-Nov      Egg Cheese Bacon Breakfast Sandw    27 Breakfast   Fall 2024
    ## 996  12-Nov                             Add Bacon    26 Breakfast   Fall 2024
    ## 997  12-Nov                              Two Eggs    26 Breakfast   Fall 2024
    ## 998  12-Nov                   Trillium Home Fries     7 Breakfast   Fall 2024
    ## 999  12-Nov                        Pancake Single     4 Breakfast   Fall 2024
    ## 1000 12-Nov                                 Toast     3 Breakfast   Fall 2024
    ## 1001 12-Nov           Create Your Pasta Bowl MEAT   112   Italian   Fall 2024
    ## 1002 12-Nov                   Pizza with Toppings    33   Italian   Fall 2024
    ## 1003 12-Nov            Create Your Pasta Bowl VEG    20   Italian   Fall 2024
    ## 1004 12-Nov                          Pizza Cheese    19   Italian   Fall 2024
    ## 1005 12-Nov                        Add Extra Meat    14   Italian   Fall 2024
    ## 1006 12-Nov                      Burrito Bowl BYO    93   Mexican   Fall 2024
    ## 1007 12-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 1008 12-Nov                        Side Guacamole     1   Mexican   Fall 2024
    ## 1009 12-Nov           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 1010 12-Nov                    Salad by the Pound    56 Salad Bar   Fall 2024
    ## 1011 12-Nov                Add Extra Protein 2.99     3 Salad Bar   Fall 2024
    ## 1012 12-Nov                             8 oz Soup    59      Soup   Fall 2024
    ## 1013 12-Nov                            Soup 12 oz    40      Soup   Fall 2024
    ## 1014 12-Nov                      Side Potato Tots    23 Grab N Go   Fall 2024
    ## 1015 13-Nov            Quesadilla Deluxe Trillium   155     Grill   Fall 2024
    ## 1016 13-Nov                     Grilled Hamburger   107     Grill   Fall 2024
    ## 1017 13-Nov                 Fried Chicken Tenders   101 Grab N Go   Fall 2024
    ## 1018 13-Nov         Burrito Una Mano Trillium BYO    66     Grill   Fall 2024
    ## 1019 13-Nov                          French Fries   169     Grill   Fall 2024
    ## 1020 13-Nov       Grilled Chicken Breast Sandwich    15     Grill   Fall 2024
    ## 1021 13-Nov                     Quesadilla Cheese    13     Grill   Fall 2024
    ## 1022 13-Nov                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 1023 13-Nov      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 1024 13-Nov                          + Beef Patty    13     Grill   Fall 2024
    ## 1025 13-Nov                    ADD Chicken Breast     7     Grill   Fall 2024
    ## 1026 13-Nov                     Black Bean Burger     2     Grill   Fall 2024
    ## 1027 13-Nov                            ADD Cheese     4     Grill   Fall 2024
    ## 1028 13-Nov                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 1029 13-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 1030 13-Nov                     1 Entree + 1 Side   201     Asian   Fall 2024
    ## 1031 13-Nov                     1 Entree + 2 Side    89     Asian   Fall 2024
    ## 1032 13-Nov                    Bowl Ramen Chicken    74     Asian   Fall 2024
    ## 1033 13-Nov                   2 Entrees + 2 Sides    18     Asian   Fall 2024
    ## 1034 13-Nov                       Bowl Ramen Tofu    17     Asian   Fall 2024
    ## 1035 13-Nov                          1 Wok Entree     5     Asian   Fall 2024
    ## 1036 13-Nov               Side Vegetarian Lo Mein     7     Asian   Fall 2024
    ## 1037 13-Nov                       Side Vegetables     2     Asian   Fall 2024
    ## 1038 13-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 1039 13-Nov              Side White or Brown Rice     3     Asian   Fall 2024
    ## 1040 13-Nov                Side Fried Spring Roll     1     Asian   Fall 2024
    ## 1041 13-Nov           Create Your Pasta Bowl MEAT   113   Italian   Fall 2024
    ## 1042 13-Nov            Create Your Pasta Bowl VEG    26   Italian   Fall 2024
    ## 1043 13-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 1044 13-Nov                          Pizza Cheese    19   Italian   Fall 2024
    ## 1045 13-Nov                        Add Extra Meat    22   Italian   Fall 2024
    ## 1046 13-Nov              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 1047 13-Nov                     Burrito Breakfast    79 Breakfast   Fall 2024
    ## 1048 13-Nov                   Small French Omelet    54 Breakfast   Fall 2024
    ## 1049 13-Nov                  Grand Slam Breakfast    23 Breakfast   Fall 2024
    ## 1050 13-Nov      Egg Cheese Sausage Breakfast San    31 Breakfast   Fall 2024
    ## 1051 13-Nov      Egg Cheese Bacon Breakfast Sandw    23 Breakfast   Fall 2024
    ## 1052 13-Nov                             Add Bacon    26 Breakfast   Fall 2024
    ## 1053 13-Nov                              Two Eggs     9 Breakfast   Fall 2024
    ## 1054 13-Nov                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1055 13-Nov                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 1056 13-Nov                                 Toast     2 Breakfast   Fall 2024
    ## 1057 13-Nov                      Burrito Bowl BYO   106   Mexican   Fall 2024
    ## 1058 13-Nov                           Single Taco    13   Mexican   Fall 2024
    ## 1059 13-Nov                    Salad by the Pound    69 Salad Bar   Fall 2024
    ## 1060 13-Nov                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 1061 13-Nov                            Soup 12 oz    45      Soup   Fall 2024
    ## 1062 13-Nov                             8 oz Soup    43      Soup   Fall 2024
    ## 1063 13-Nov                      Side Potato Tots    17 Grab N Go   Fall 2024
    ## 1064 14-Nov            Quesadilla Deluxe Trillium   178     Grill   Fall 2024
    ## 1065 14-Nov                     Grilled Hamburger    99     Grill   Fall 2024
    ## 1066 14-Nov                 Fried Chicken Tenders   108 Grab N Go   Fall 2024
    ## 1067 14-Nov         Burrito Una Mano Trillium BYO    59     Grill   Fall 2024
    ## 1068 14-Nov                          French Fries   175     Grill   Fall 2024
    ## 1069 14-Nov       Grilled Chicken Breast Sandwich    20     Grill   Fall 2024
    ## 1070 14-Nov      Trillium Grill Impossible Burger    15     Grill   Fall 2024
    ## 1071 14-Nov                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 1072 14-Nov                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 1073 14-Nov                          + Beef Patty    10     Grill   Fall 2024
    ## 1074 14-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 1075 14-Nov                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 1076 14-Nov                            ADD Cheese    12     Grill   Fall 2024
    ## 1077 14-Nov                    Sweet Potato Fries     1     Grill   Fall 2024
    ## 1078 14-Nov                           Add Egg .99     1     Grill   Fall 2024
    ## 1079 14-Nov                     1 Entree + 1 Side   175     Asian   Fall 2024
    ## 1080 14-Nov                     1 Entree + 2 Side    84     Asian   Fall 2024
    ## 1081 14-Nov                    Bowl Ramen Chicken    78     Asian   Fall 2024
    ## 1082 14-Nov                   2 Entrees + 2 Sides    33     Asian   Fall 2024
    ## 1083 14-Nov                       Bowl Ramen Tofu    21     Asian   Fall 2024
    ## 1084 14-Nov               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 1085 14-Nov                          1 Wok Entree     4     Asian   Fall 2024
    ## 1086 14-Nov              Side White or Brown Rice     8     Asian   Fall 2024
    ## 1087 14-Nov                Side Fried Spring Roll     2     Asian   Fall 2024
    ## 1088 14-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1089 14-Nov                     Burrito Breakfast    82 Breakfast   Fall 2024
    ## 1090 14-Nov                   Small French Omelet    56 Breakfast   Fall 2024
    ## 1091 14-Nov                  Grand Slam Breakfast    18 Breakfast   Fall 2024
    ## 1092 14-Nov      Egg Cheese Sausage Breakfast San    26 Breakfast   Fall 2024
    ## 1093 14-Nov      Egg Cheese Bacon Breakfast Sandw    19 Breakfast   Fall 2024
    ## 1094 14-Nov                             Add Bacon    33 Breakfast   Fall 2024
    ## 1095 14-Nov                              Two Eggs    18 Breakfast   Fall 2024
    ## 1096 14-Nov                        Pancake Single     5 Breakfast   Fall 2024
    ## 1097 14-Nov                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 1098 14-Nov                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 1099 14-Nov           Create Your Pasta Bowl MEAT   103   Italian   Fall 2024
    ## 1100 14-Nov            Create Your Pasta Bowl VEG    31   Italian   Fall 2024
    ## 1101 14-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 1102 14-Nov                          Pizza Cheese    22   Italian   Fall 2024
    ## 1103 14-Nov                        Add Extra Meat    16   Italian   Fall 2024
    ## 1104 14-Nov                      Burrito Bowl BYO    86   Mexican   Fall 2024
    ## 1105 14-Nov                           Single Taco     5   Mexican   Fall 2024
    ## 1106 14-Nov                        Side Guacamole     3   Mexican   Fall 2024
    ## 1107 14-Nov                            Side Salsa     1   Mexican   Fall 2024
    ## 1108 14-Nov                    Salad by the Pound    63 Salad Bar   Fall 2024
    ## 1109 14-Nov                Add Extra Protein 2.99     4 Salad Bar   Fall 2024
    ## 1110 14-Nov                             8 oz Soup    43      Soup   Fall 2024
    ## 1111 14-Nov                            Soup 12 oz    37      Soup   Fall 2024
    ## 1112 14-Nov                      Side Potato Tots    23 Grab N Go   Fall 2024
    ## 1113 15-Nov            Quesadilla Deluxe Trillium   125     Grill   Fall 2024
    ## 1114 15-Nov                     Grilled Hamburger    72     Grill   Fall 2024
    ## 1115 15-Nov         Burrito Una Mano Trillium BYO    43     Grill   Fall 2024
    ## 1116 15-Nov                 Fried Chicken Tenders    47 Grab N Go   Fall 2024
    ## 1117 15-Nov                          French Fries    92     Grill   Fall 2024
    ## 1118 15-Nov                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 1119 15-Nov      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 1120 15-Nov       Grilled Chicken Breast Sandwich     7     Grill   Fall 2024
    ## 1121 15-Nov                     Black Bean Burger     4     Grill   Fall 2024
    ## 1122 15-Nov                  Seared Salmon Burger     4     Grill   Fall 2024
    ## 1123 15-Nov                          + Beef Patty     8     Grill   Fall 2024
    ## 1124 15-Nov                    ADD Chicken Breast     4     Grill   Fall 2024
    ## 1125 15-Nov             ADD Burger Salmon Grilled     3     Grill   Fall 2024
    ## 1126 15-Nov                            ADD Cheese     6     Grill   Fall 2024
    ## 1127 15-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 1128 15-Nov                     1 Entree + 1 Side    94     Asian   Fall 2024
    ## 1129 15-Nov                     1 Entree + 2 Side    64     Asian   Fall 2024
    ## 1130 15-Nov                    Bowl Ramen Chicken    59     Asian   Fall 2024
    ## 1131 15-Nov                   2 Entrees + 2 Sides    18     Asian   Fall 2024
    ## 1132 15-Nov                       Bowl Ramen Tofu    17     Asian   Fall 2024
    ## 1133 15-Nov               Side Vegetarian Lo Mein     6     Asian   Fall 2024
    ## 1134 15-Nov                          1 Wok Entree     3     Asian   Fall 2024
    ## 1135 15-Nov       Side Vegetarian Fried Rice with     4     Asian   Fall 2024
    ## 1136 15-Nov              Side White or Brown Rice     6     Asian   Fall 2024
    ## 1137 15-Nov                       Side Vegetables     3     Asian   Fall 2024
    ## 1138 15-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 1139 15-Nov                     Burrito Breakfast    81 Breakfast   Fall 2024
    ## 1140 15-Nov                   Small French Omelet    39 Breakfast   Fall 2024
    ## 1141 15-Nov                  Grand Slam Breakfast    20 Breakfast   Fall 2024
    ## 1142 15-Nov      Egg Cheese Sausage Breakfast San    28 Breakfast   Fall 2024
    ## 1143 15-Nov      Egg Cheese Bacon Breakfast Sandw    19 Breakfast   Fall 2024
    ## 1144 15-Nov                             Add Bacon    17 Breakfast   Fall 2024
    ## 1145 15-Nov                              Two Eggs     7 Breakfast   Fall 2024
    ## 1146 15-Nov                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 1147 15-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 1148 15-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1149 15-Nov           Create Your Pasta Bowl MEAT    79   Italian   Fall 2024
    ## 1150 15-Nov                   Pizza with Toppings    23   Italian   Fall 2024
    ## 1151 15-Nov            Create Your Pasta Bowl VEG    15   Italian   Fall 2024
    ## 1152 15-Nov                          Pizza Cheese    12   Italian   Fall 2024
    ## 1153 15-Nov                        Add Extra Meat    19   Italian   Fall 2024
    ## 1154 15-Nov              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 1155 15-Nov                      Burrito Bowl BYO    56   Mexican   Fall 2024
    ## 1156 15-Nov                           Single Taco     7   Mexican   Fall 2024
    ## 1157 15-Nov                       Side Sour Cream     1   Mexican   Fall 2024
    ## 1158 15-Nov                    Salad by the Pound    39 Salad Bar   Fall 2024
    ## 1159 15-Nov                            Soup 12 oz    27      Soup   Fall 2024
    ## 1160 15-Nov                             8 oz Soup    28      Soup   Fall 2024
    ## 1161 15-Nov                      Side Potato Tots    23 Grab N Go   Fall 2024
    ## 1162 18-Nov            Quesadilla Deluxe Trillium   155     Grill   Fall 2024
    ## 1163 18-Nov                     Grilled Hamburger    82     Grill   Fall 2024
    ## 1164 18-Nov                 Fried Chicken Tenders    84 Grab N Go   Fall 2024
    ## 1165 18-Nov         Burrito Una Mano Trillium BYO    54     Grill   Fall 2024
    ## 1166 18-Nov                          French Fries   135     Grill   Fall 2024
    ## 1167 18-Nov       Grilled Chicken Breast Sandwich    15     Grill   Fall 2024
    ## 1168 18-Nov      Trillium Grill Impossible Burger    12     Grill   Fall 2024
    ## 1169 18-Nov                  Seared Salmon Burger    14     Grill   Fall 2024
    ## 1170 18-Nov                     Black Bean Burger     7     Grill   Fall 2024
    ## 1171 18-Nov                          + Beef Patty    16     Grill   Fall 2024
    ## 1172 18-Nov                     Quesadilla Cheese     5     Grill   Fall 2024
    ## 1173 18-Nov                    ADD Chicken Breast     6     Grill   Fall 2024
    ## 1174 18-Nov                   Add Sausage 2 Patty     4     Grill   Fall 2024
    ## 1175 18-Nov                            ADD Cheese    11     Grill   Fall 2024
    ## 1176 18-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 1177 18-Nov                     1 Entree + 1 Side   168     Asian   Fall 2024
    ## 1178 18-Nov                     1 Entree + 2 Side    76     Asian   Fall 2024
    ## 1179 18-Nov                    Bowl Ramen Chicken    66     Asian   Fall 2024
    ## 1180 18-Nov                   2 Entrees + 2 Sides    22     Asian   Fall 2024
    ## 1181 18-Nov                       Bowl Ramen Tofu    11     Asian   Fall 2024
    ## 1182 18-Nov                          1 Wok Entree     5     Asian   Fall 2024
    ## 1183 18-Nov               Side Vegetarian Lo Mein     6     Asian   Fall 2024
    ## 1184 18-Nov              Side White or Brown Rice     4     Asian   Fall 2024
    ## 1185 18-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 1186 18-Nov       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1187 18-Nov           Create Your Pasta Bowl MEAT   116   Italian   Fall 2024
    ## 1188 18-Nov            Create Your Pasta Bowl VEG    30   Italian   Fall 2024
    ## 1189 18-Nov                   Pizza with Toppings    35   Italian   Fall 2024
    ## 1190 18-Nov                          Pizza Cheese    21   Italian   Fall 2024
    ## 1191 18-Nov                        Add Extra Meat    18   Italian   Fall 2024
    ## 1192 18-Nov                     Burrito Breakfast    99 Breakfast   Fall 2024
    ## 1193 18-Nov                   Small French Omelet    62 Breakfast   Fall 2024
    ## 1194 18-Nov      Egg Cheese Sausage Breakfast San    25 Breakfast   Fall 2024
    ## 1195 18-Nov      Egg Cheese Bacon Breakfast Sandw    19 Breakfast   Fall 2024
    ## 1196 18-Nov                  Grand Slam Breakfast    10 Breakfast   Fall 2024
    ## 1197 18-Nov                             Add Bacon    28 Breakfast   Fall 2024
    ## 1198 18-Nov                              Two Eggs    14 Breakfast   Fall 2024
    ## 1199 18-Nov                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 1200 18-Nov                        Pancake Single     3 Breakfast   Fall 2024
    ## 1201 18-Nov                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 1202 18-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 1203 18-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1204 18-Nov                      Burrito Bowl BYO   109   Mexican   Fall 2024
    ## 1205 18-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 1206 18-Nov                        Side Guacamole     1   Mexican   Fall 2024
    ## 1207 18-Nov                       Side Sour Cream     1   Mexican   Fall 2024
    ## 1208 18-Nov                    Salad by the Pound    55 Salad Bar   Fall 2024
    ## 1209 18-Nov                            Soup 12 oz    49      Soup   Fall 2024
    ## 1210 18-Nov                             8 oz Soup    39      Soup   Fall 2024
    ## 1211 18-Nov                      Side Potato Tots    16 Grab N Go   Fall 2024
    ## 1212 19-Nov            Quesadilla Deluxe Trillium   189     Grill   Fall 2024
    ## 1213 19-Nov                     Grilled Hamburger   122     Grill   Fall 2024
    ## 1214 19-Nov                 Fried Chicken Tenders    99 Grab N Go   Fall 2024
    ## 1215 19-Nov         Burrito Una Mano Trillium BYO    69     Grill   Fall 2024
    ## 1216 19-Nov                          French Fries   143     Grill   Fall 2024
    ## 1217 19-Nov       Grilled Chicken Breast Sandwich    26     Grill   Fall 2024
    ## 1218 19-Nov                     Quesadilla Cheese    22     Grill   Fall 2024
    ## 1219 19-Nov      Trillium Grill Impossible Burger    10     Grill   Fall 2024
    ## 1220 19-Nov                    Sweet Potato Fries    31     Grill   Fall 2024
    ## 1221 19-Nov                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 1222 19-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 1223 19-Nov                    ADD Chicken Breast     7     Grill   Fall 2024
    ## 1224 19-Nov                          + Beef Patty     7     Grill   Fall 2024
    ## 1225 19-Nov                           Add Egg .99     5     Grill   Fall 2024
    ## 1226 19-Nov             ADD Burger Salmon Grilled     1     Grill   Fall 2024
    ## 1227 19-Nov                            ADD Cheese     5     Grill   Fall 2024
    ## 1228 19-Nov                     1 Entree + 1 Side   185     Asian   Fall 2024
    ## 1229 19-Nov                     1 Entree + 2 Side    92     Asian   Fall 2024
    ## 1230 19-Nov                    Bowl Ramen Chicken    70     Asian   Fall 2024
    ## 1231 19-Nov                   2 Entrees + 2 Sides    33     Asian   Fall 2024
    ## 1232 19-Nov                       Bowl Ramen Tofu    28     Asian   Fall 2024
    ## 1233 19-Nov               Side Vegetarian Lo Mein    16     Asian   Fall 2024
    ## 1234 19-Nov                          1 Wok Entree     7     Asian   Fall 2024
    ## 1235 19-Nov              Side White or Brown Rice    12     Asian   Fall 2024
    ## 1236 19-Nov           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 1237 19-Nov       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1238 19-Nov           Create Your Pasta Bowl MEAT   115   Italian   Fall 2024
    ## 1239 19-Nov            Create Your Pasta Bowl VEG    28   Italian   Fall 2024
    ## 1240 19-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 1241 19-Nov                          Pizza Cheese    22   Italian   Fall 2024
    ## 1242 19-Nov                        Add Extra Meat    18   Italian   Fall 2024
    ## 1243 19-Nov                     Burrito Breakfast    95 Breakfast   Fall 2024
    ## 1244 19-Nov                   Small French Omelet    47 Breakfast   Fall 2024
    ## 1245 19-Nov      Egg Cheese Sausage Breakfast San    30 Breakfast   Fall 2024
    ## 1246 19-Nov      Egg Cheese Bacon Breakfast Sandw    23 Breakfast   Fall 2024
    ## 1247 19-Nov                  Grand Slam Breakfast    12 Breakfast   Fall 2024
    ## 1248 19-Nov                             Add Bacon    28 Breakfast   Fall 2024
    ## 1249 19-Nov                              Two Eggs    14 Breakfast   Fall 2024
    ## 1250 19-Nov                        Pancake Single     8 Breakfast   Fall 2024
    ## 1251 19-Nov                   Trillium Home Fries     2 Breakfast   Fall 2024
    ## 1252 19-Nov                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 1253 19-Nov                      Burrito Bowl BYO   110   Mexican   Fall 2024
    ## 1254 19-Nov                           Single Taco    11   Mexican   Fall 2024
    ## 1255 19-Nov                        Side Guacamole     3   Mexican   Fall 2024
    ## 1256 19-Nov           Add Extra Toppings Una Mano     3   Mexican   Fall 2024
    ## 1257 19-Nov                    Salad by the Pound    75 Salad Bar   Fall 2024
    ## 1258 19-Nov                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 1259 19-Nov                            Soup 12 oz    52      Soup   Fall 2024
    ## 1260 19-Nov                             8 oz Soup    55      Soup   Fall 2024
    ## 1261 19-Nov                      Side Potato Tots    21 Grab N Go   Fall 2024
    ## 1262 20-Nov            Quesadilla Deluxe Trillium   162     Grill   Fall 2024
    ## 1263 20-Nov                     Grilled Hamburger    92     Grill   Fall 2024
    ## 1264 20-Nov                 Fried Chicken Tenders    93 Grab N Go   Fall 2024
    ## 1265 20-Nov         Burrito Una Mano Trillium BYO    72     Grill   Fall 2024
    ## 1266 20-Nov                          French Fries   124     Grill   Fall 2024
    ## 1267 20-Nov                  Seared Salmon Burger    14     Grill   Fall 2024
    ## 1268 20-Nov       Grilled Chicken Breast Sandwich    13     Grill   Fall 2024
    ## 1269 20-Nov                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 1270 20-Nov      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 1271 20-Nov                    Sweet Potato Fries    24     Grill   Fall 2024
    ## 1272 20-Nov                    ADD Chicken Breast     9     Grill   Fall 2024
    ## 1273 20-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 1274 20-Nov                          + Beef Patty     5     Grill   Fall 2024
    ## 1275 20-Nov                            ADD Cheese     6     Grill   Fall 2024
    ## 1276 20-Nov                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 1277 20-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 1278 20-Nov                     1 Entree + 1 Side   176     Asian   Fall 2024
    ## 1279 20-Nov                     1 Entree + 2 Side    97     Asian   Fall 2024
    ## 1280 20-Nov                    Bowl Ramen Chicken    80     Asian   Fall 2024
    ## 1281 20-Nov                   2 Entrees + 2 Sides    21     Asian   Fall 2024
    ## 1282 20-Nov                       Bowl Ramen Tofu    19     Asian   Fall 2024
    ## 1283 20-Nov               Side Vegetarian Lo Mein    13     Asian   Fall 2024
    ## 1284 20-Nov              Side White or Brown Rice     7     Asian   Fall 2024
    ## 1285 20-Nov                          1 Wok Entree     2     Asian   Fall 2024
    ## 1286 20-Nov                       Side Vegetables     2     Asian   Fall 2024
    ## 1287 20-Nov           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1288 20-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1289 20-Nov           Create Your Pasta Bowl MEAT   143   Italian   Fall 2024
    ## 1290 20-Nov            Create Your Pasta Bowl VEG    32   Italian   Fall 2024
    ## 1291 20-Nov                        Add Extra Meat    20   Italian   Fall 2024
    ## 1292 20-Nov                          Pizza Cheese    10   Italian   Fall 2024
    ## 1293 20-Nov                   Pizza with Toppings     8   Italian   Fall 2024
    ## 1294 20-Nov              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 1295 20-Nov                     Burrito Breakfast    91 Breakfast   Fall 2024
    ## 1296 20-Nov                   Small French Omelet    55 Breakfast   Fall 2024
    ## 1297 20-Nov                  Grand Slam Breakfast    16 Breakfast   Fall 2024
    ## 1298 20-Nov      Egg Cheese Sausage Breakfast San    24 Breakfast   Fall 2024
    ## 1299 20-Nov      Egg Cheese Bacon Breakfast Sandw    20 Breakfast   Fall 2024
    ## 1300 20-Nov                             Add Bacon    22 Breakfast   Fall 2024
    ## 1301 20-Nov                              Two Eggs     7 Breakfast   Fall 2024
    ## 1302 20-Nov                        Pancake Single     4 Breakfast   Fall 2024
    ## 1303 20-Nov                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1304 20-Nov                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 1305 20-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 1306 20-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1307 20-Nov                      Burrito Bowl BYO    88   Mexican   Fall 2024
    ## 1308 20-Nov                           Single Taco     5   Mexican   Fall 2024
    ## 1309 20-Nov           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 1310 20-Nov                    Salad by the Pound    71 Salad Bar   Fall 2024
    ## 1311 20-Nov                            Soup 12 oz    56      Soup   Fall 2024
    ## 1312 20-Nov                             8 oz Soup    35      Soup   Fall 2024
    ## 1313 20-Nov                      Side Potato Tots    11 Grab N Go   Fall 2024
    ## 1314 21-Nov            Quesadilla Deluxe Trillium   197     Grill   Fall 2024
    ## 1315 21-Nov                     Grilled Hamburger   109     Grill   Fall 2024
    ## 1316 21-Nov                 Fried Chicken Tenders    93 Grab N Go   Fall 2024
    ## 1317 21-Nov         Burrito Una Mano Trillium BYO    66     Grill   Fall 2024
    ## 1318 21-Nov                          French Fries   113     Grill   Fall 2024
    ## 1319 21-Nov       Grilled Chicken Breast Sandwich    13     Grill   Fall 2024
    ## 1320 21-Nov                    Sweet Potato Fries    32     Grill   Fall 2024
    ## 1321 21-Nov      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 1322 21-Nov                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 1323 21-Nov                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 1324 21-Nov                     Black Bean Burger     4     Grill   Fall 2024
    ## 1325 21-Nov                          + Beef Patty     6     Grill   Fall 2024
    ## 1326 21-Nov                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 1327 21-Nov                   Add Sausage 2 Patty     4     Grill   Fall 2024
    ## 1328 21-Nov             ADD Burger Salmon Grilled     1     Grill   Fall 2024
    ## 1329 21-Nov                            ADD Cheese     6     Grill   Fall 2024
    ## 1330 21-Nov                           Add Egg .99     3     Grill   Fall 2024
    ## 1331 21-Nov                     1 Entree + 1 Side   209     Asian   Fall 2024
    ## 1332 21-Nov                     1 Entree + 2 Side    85     Asian   Fall 2024
    ## 1333 21-Nov                    Bowl Ramen Chicken    77     Asian   Fall 2024
    ## 1334 21-Nov                   2 Entrees + 2 Sides    25     Asian   Fall 2024
    ## 1335 21-Nov                       Bowl Ramen Tofu    20     Asian   Fall 2024
    ## 1336 21-Nov               Side Vegetarian Lo Mein    12     Asian   Fall 2024
    ## 1337 21-Nov                          1 Wok Entree     3     Asian   Fall 2024
    ## 1338 21-Nov              Side White or Brown Rice     5     Asian   Fall 2024
    ## 1339 21-Nov       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1340 21-Nov           Create Your Pasta Bowl MEAT   131   Italian   Fall 2024
    ## 1341 21-Nov                   Pizza with Toppings    38   Italian   Fall 2024
    ## 1342 21-Nov            Create Your Pasta Bowl VEG    24   Italian   Fall 2024
    ## 1343 21-Nov                          Pizza Cheese    22   Italian   Fall 2024
    ## 1344 21-Nov                        Add Extra Meat    15   Italian   Fall 2024
    ## 1345 21-Nov                     Burrito Breakfast    75 Breakfast   Fall 2024
    ## 1346 21-Nov                   Small French Omelet    40 Breakfast   Fall 2024
    ## 1347 21-Nov                  Grand Slam Breakfast    14 Breakfast   Fall 2024
    ## 1348 21-Nov      Egg Cheese Bacon Breakfast Sandw    23 Breakfast   Fall 2024
    ## 1349 21-Nov      Egg Cheese Sausage Breakfast San    21 Breakfast   Fall 2024
    ## 1350 21-Nov                             Add Bacon    33 Breakfast   Fall 2024
    ## 1351 21-Nov                              Two Eggs    23 Breakfast   Fall 2024
    ## 1352 21-Nov                   Trillium Home Fries     4 Breakfast   Fall 2024
    ## 1353 21-Nov                        Pancake Single     2 Breakfast   Fall 2024
    ## 1354 21-Nov                        2 Slices Toast     5 Breakfast   Fall 2024
    ## 1355 21-Nov                                 Toast     2 Breakfast   Fall 2024
    ## 1356 21-Nov                      Burrito Bowl BYO    92   Mexican   Fall 2024
    ## 1357 21-Nov                           Single Taco     4   Mexican   Fall 2024
    ## 1358 21-Nov                        Side Guacamole     2   Mexican   Fall 2024
    ## 1359 21-Nov           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 1360 21-Nov                            Soup 12 oz    45      Soup   Fall 2024
    ## 1361 21-Nov                             8 oz Soup    36      Soup   Fall 2024
    ## 1362 21-Nov                    Salad by the Pound    39 Salad Bar   Fall 2024
    ## 1363 21-Nov                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 1364 21-Nov                      Side Potato Tots    35 Grab N Go   Fall 2024
    ## 1365 22-Nov            Quesadilla Deluxe Trillium   103     Grill   Fall 2024
    ## 1366 22-Nov                     Grilled Hamburger    51     Grill   Fall 2024
    ## 1367 22-Nov                 Fried Chicken Tenders    48 Grab N Go   Fall 2024
    ## 1368 22-Nov         Burrito Una Mano Trillium BYO    36     Grill   Fall 2024
    ## 1369 22-Nov                          French Fries    81     Grill   Fall 2024
    ## 1370 22-Nov       Grilled Chicken Breast Sandwich    15     Grill   Fall 2024
    ## 1371 22-Nov      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 1372 22-Nov                     Quesadilla Cheese     5     Grill   Fall 2024
    ## 1373 22-Nov                    Sweet Potato Fries    13     Grill   Fall 2024
    ## 1374 22-Nov                          + Beef Patty     7     Grill   Fall 2024
    ## 1375 22-Nov                   Add Sausage 2 Patty     5     Grill   Fall 2024
    ## 1376 22-Nov                     Black Bean Burger     1     Grill   Fall 2024
    ## 1377 22-Nov                  Seared Salmon Burger     1     Grill   Fall 2024
    ## 1378 22-Nov                            ADD Cheese     6     Grill   Fall 2024
    ## 1379 22-Nov                           Add Egg .99     2     Grill   Fall 2024
    ## 1380 22-Nov                     1 Entree + 1 Side    90     Asian   Fall 2024
    ## 1381 22-Nov                     1 Entree + 2 Side    45     Asian   Fall 2024
    ## 1382 22-Nov                    Bowl Ramen Chicken    49     Asian   Fall 2024
    ## 1383 22-Nov                   2 Entrees + 2 Sides    10     Asian   Fall 2024
    ## 1384 22-Nov                       Bowl Ramen Tofu     8     Asian   Fall 2024
    ## 1385 22-Nov               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 1386 22-Nov              Side White or Brown Rice    10     Asian   Fall 2024
    ## 1387 22-Nov                          1 Wok Entree     2     Asian   Fall 2024
    ## 1388 22-Nov           Side Vegetable Spring Rolls     3     Asian   Fall 2024
    ## 1389 22-Nov                       Side Vegetables     2     Asian   Fall 2024
    ## 1390 22-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1391 22-Nov                     Burrito Breakfast    66 Breakfast   Fall 2024
    ## 1392 22-Nov                   Small French Omelet    27 Breakfast   Fall 2024
    ## 1393 22-Nov      Egg Cheese Bacon Breakfast Sandw    25 Breakfast   Fall 2024
    ## 1394 22-Nov                  Grand Slam Breakfast    13 Breakfast   Fall 2024
    ## 1395 22-Nov      Egg Cheese Sausage Breakfast San    22 Breakfast   Fall 2024
    ## 1396 22-Nov                              Two Eggs    15 Breakfast   Fall 2024
    ## 1397 22-Nov                             Add Bacon    14 Breakfast   Fall 2024
    ## 1398 22-Nov                        Pancake Single     5 Breakfast   Fall 2024
    ## 1399 22-Nov                        2 Slices Toast     5 Breakfast   Fall 2024
    ## 1400 22-Nov                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1401 22-Nov                              PC Jelly     2 Breakfast   Fall 2024
    ## 1402 22-Nov                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1403 22-Nov           Create Your Pasta Bowl MEAT    70   Italian   Fall 2024
    ## 1404 22-Nov                   Pizza with Toppings    21   Italian   Fall 2024
    ## 1405 22-Nov                          Pizza Cheese    18   Italian   Fall 2024
    ## 1406 22-Nov            Create Your Pasta Bowl VEG     9   Italian   Fall 2024
    ## 1407 22-Nov                        Add Extra Meat    11   Italian   Fall 2024
    ## 1408 22-Nov                      Burrito Bowl BYO    51   Mexican   Fall 2024
    ## 1409 22-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 1410 22-Nov                            Soup 12 oz    41      Soup   Fall 2024
    ## 1411 22-Nov                             8 oz Soup    32      Soup   Fall 2024
    ## 1412 22-Nov                    Salad by the Pound    30 Salad Bar   Fall 2024
    ## 1413 22-Nov                      Side Potato Tots    19 Grab N Go   Fall 2024
    ## 1414 25-Nov            Quesadilla Deluxe Trillium   120     Grill   Fall 2024
    ## 1415 25-Nov                     Grilled Hamburger    67     Grill   Fall 2024
    ## 1416 25-Nov         Burrito Una Mano Trillium BYO    45     Grill   Fall 2024
    ## 1417 25-Nov                 Fried Chicken Tenders    45 Grab N Go   Fall 2024
    ## 1418 25-Nov                          French Fries    77     Grill   Fall 2024
    ## 1419 25-Nov       Grilled Chicken Breast Sandwich    14     Grill   Fall 2024
    ## 1420 25-Nov                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 1421 25-Nov                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 1422 25-Nov                    Sweet Potato Fries    22     Grill   Fall 2024
    ## 1423 25-Nov      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 1424 25-Nov                          + Beef Patty     9     Grill   Fall 2024
    ## 1425 25-Nov                    ADD Chicken Breast     7     Grill   Fall 2024
    ## 1426 25-Nov                     Black Bean Burger     2     Grill   Fall 2024
    ## 1427 25-Nov             ADD Burger Salmon Grilled     2     Grill   Fall 2024
    ## 1428 25-Nov                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 1429 25-Nov                            ADD Cheese     5     Grill   Fall 2024
    ## 1430 25-Nov                           Add Egg .99     3     Grill   Fall 2024
    ## 1431 25-Nov           Add Impossible Burger Patty     0     Grill   Fall 2024
    ## 1432 25-Nov                     1 Entree + 1 Side   104     Asian   Fall 2024
    ## 1433 25-Nov                    Bowl Ramen Chicken    56     Asian   Fall 2024
    ## 1434 25-Nov                     1 Entree + 2 Side    47     Asian   Fall 2024
    ## 1435 25-Nov                   2 Entrees + 2 Sides    10     Asian   Fall 2024
    ## 1436 25-Nov                       Bowl Ramen Tofu    10     Asian   Fall 2024
    ## 1437 25-Nov               Side Vegetarian Lo Mein     8     Asian   Fall 2024
    ## 1438 25-Nov              Side White or Brown Rice     6     Asian   Fall 2024
    ## 1439 25-Nov                       Side Vegetables     3     Asian   Fall 2024
    ## 1440 25-Nov                          1 Wok Entree     1     Asian   Fall 2024
    ## 1441 25-Nov       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1442 25-Nov           Create Your Pasta Bowl MEAT    55   Italian   Fall 2024
    ## 1443 25-Nov            Create Your Pasta Bowl VEG    20   Italian   Fall 2024
    ## 1444 25-Nov                   Pizza with Toppings    23   Italian   Fall 2024
    ## 1445 25-Nov                          Pizza Cheese    11   Italian   Fall 2024
    ## 1446 25-Nov                        Add Extra Meat    10   Italian   Fall 2024
    ## 1447 25-Nov                     Burrito Breakfast    31 Breakfast   Fall 2024
    ## 1448 25-Nov                   Small French Omelet    27 Breakfast   Fall 2024
    ## 1449 25-Nov      Egg Cheese Sausage Breakfast San    17 Breakfast   Fall 2024
    ## 1450 25-Nov                  Grand Slam Breakfast     9 Breakfast   Fall 2024
    ## 1451 25-Nov      Egg Cheese Bacon Breakfast Sandw    10 Breakfast   Fall 2024
    ## 1452 25-Nov                             Add Bacon    17 Breakfast   Fall 2024
    ## 1453 25-Nov                              Two Eggs     4 Breakfast   Fall 2024
    ## 1454 25-Nov                        Pancake Single     3 Breakfast   Fall 2024
    ## 1455 25-Nov                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1456 25-Nov                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 1457 25-Nov                      Burrito Bowl BYO    43   Mexican   Fall 2024
    ## 1458 25-Nov                           Single Taco     6   Mexican   Fall 2024
    ## 1459 25-Nov           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 1460 25-Nov                    Salad by the Pound    46 Salad Bar   Fall 2024
    ## 1461 25-Nov                            Soup 12 oz    24      Soup   Fall 2024
    ## 1462 25-Nov                             8 oz Soup    14      Soup   Fall 2024
    ## 1463 25-Nov                      Side Potato Tots     4 Grab N Go   Fall 2024
    ## 1464 26-Nov            Quesadilla Deluxe Trillium   121     Grill   Fall 2024
    ## 1465 26-Nov                     Grilled Hamburger    52     Grill   Fall 2024
    ## 1466 26-Nov                 Fried Chicken Tenders    42 Grab N Go   Fall 2024
    ## 1467 26-Nov         Burrito Una Mano Trillium BYO    31     Grill   Fall 2024
    ## 1468 26-Nov                          French Fries    71     Grill   Fall 2024
    ## 1469 26-Nov                  Seared Salmon Burger    12     Grill   Fall 2024
    ## 1470 26-Nov                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 1471 26-Nov                    Sweet Potato Fries    20     Grill   Fall 2024
    ## 1472 26-Nov      Trillium Grill Impossible Burger     4     Grill   Fall 2024
    ## 1473 26-Nov       Grilled Chicken Breast Sandwich     4     Grill   Fall 2024
    ## 1474 26-Nov                     Black Bean Burger     3     Grill   Fall 2024
    ## 1475 26-Nov                    ADD Chicken Breast     2     Grill   Fall 2024
    ## 1476 26-Nov                          + Beef Patty     2     Grill   Fall 2024
    ## 1477 26-Nov                            ADD Cheese     5     Grill   Fall 2024
    ## 1478 26-Nov                     1 Entree + 1 Side    77     Asian   Fall 2024
    ## 1479 26-Nov                     1 Entree + 2 Side    46     Asian   Fall 2024
    ## 1480 26-Nov                    Bowl Ramen Chicken    45     Asian   Fall 2024
    ## 1481 26-Nov                   2 Entrees + 2 Sides    17     Asian   Fall 2024
    ## 1482 26-Nov                       Bowl Ramen Tofu     6     Asian   Fall 2024
    ## 1483 26-Nov               Side Vegetarian Lo Mein     3     Asian   Fall 2024
    ## 1484 26-Nov                          1 Wok Entree     1     Asian   Fall 2024
    ## 1485 26-Nov              Side White or Brown Rice     3     Asian   Fall 2024
    ## 1486 26-Nov                       Side Vegetables     1     Asian   Fall 2024
    ## 1487 26-Nov                     Burrito Breakfast    41 Breakfast   Fall 2024
    ## 1488 26-Nov                   Small French Omelet    15 Breakfast   Fall 2024
    ## 1489 26-Nov                  Grand Slam Breakfast     7 Breakfast   Fall 2024
    ## 1490 26-Nov                             Add Bacon    12 Breakfast   Fall 2024
    ## 1491 26-Nov      Egg Cheese Sausage Breakfast San     4 Breakfast   Fall 2024
    ## 1492 26-Nov                              Two Eggs     5 Breakfast   Fall 2024
    ## 1493 26-Nov                        Pancake Single     1 Breakfast   Fall 2024
    ## 1494 26-Nov                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 1495 26-Nov                                 Toast     1 Breakfast   Fall 2024
    ## 1496 26-Nov           Create Your Pasta Bowl MEAT    29   Italian   Fall 2024
    ## 1497 26-Nov                   Pizza with Toppings    14   Italian   Fall 2024
    ## 1498 26-Nov            Create Your Pasta Bowl VEG     6   Italian   Fall 2024
    ## 1499 26-Nov                          Pizza Cheese     6   Italian   Fall 2024
    ## 1500 26-Nov                        Add Extra Meat     2   Italian   Fall 2024
    ## 1501 26-Nov                      Burrito Bowl BYO    42   Mexican   Fall 2024
    ## 1502 26-Nov                           Single Taco     3   Mexican   Fall 2024
    ## 1503 26-Nov           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 1504 26-Nov                    Salad by the Pound    31 Salad Bar   Fall 2024
    ## 1505 26-Nov                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 1506 26-Nov                            Soup 12 oz    25      Soup   Fall 2024
    ## 1507 26-Nov                             8 oz Soup    24      Soup   Fall 2024
    ## 1508 26-Nov                      Side Potato Tots    12 Grab N Go   Fall 2024
    ## 1509  2-Dec            Quesadilla Deluxe Trillium   144     Grill   Fall 2024
    ## 1510  2-Dec                     Grilled Hamburger    97     Grill   Fall 2024
    ## 1511  2-Dec         Burrito Una Mano Trillium BYO    48     Grill   Fall 2024
    ## 1512  2-Dec                 Fried Chicken Tenders    60 Grab N Go   Fall 2024
    ## 1513  2-Dec                          French Fries   101     Grill   Fall 2024
    ## 1514  2-Dec                     Quesadilla Cheese    16     Grill   Fall 2024
    ## 1515  2-Dec                  Seared Salmon Burger     9     Grill   Fall 2024
    ## 1516  2-Dec                    Sweet Potato Fries    23     Grill   Fall 2024
    ## 1517  2-Dec                          + Beef Patty    19     Grill   Fall 2024
    ## 1518  2-Dec                     Black Bean Burger     6     Grill   Fall 2024
    ## 1519  2-Dec       Grilled Chicken Breast Sandwich     6     Grill   Fall 2024
    ## 1520  2-Dec                    ADD Chicken Breast     8     Grill   Fall 2024
    ## 1521  2-Dec                            ADD Cheese    10     Grill   Fall 2024
    ## 1522  2-Dec                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 1523  2-Dec                     1 Entree + 1 Side   169     Asian   Fall 2024
    ## 1524  2-Dec                    Bowl Ramen Chicken    89     Asian   Fall 2024
    ## 1525  2-Dec                     1 Entree + 2 Side    69     Asian   Fall 2024
    ## 1526  2-Dec                   2 Entrees + 2 Sides    16     Asian   Fall 2024
    ## 1527  2-Dec                       Bowl Ramen Tofu    19     Asian   Fall 2024
    ## 1528  2-Dec                          1 Wok Entree     9     Asian   Fall 2024
    ## 1529  2-Dec                    Bowl Ramen Chicken     5     Asian   Fall 2024
    ## 1530  2-Dec               Side Vegetarian Lo Mein     9     Asian   Fall 2024
    ## 1531  2-Dec              Side White or Brown Rice     9     Asian   Fall 2024
    ## 1532  2-Dec                       Side Vegetables     1     Asian   Fall 2024
    ## 1533  2-Dec           Create Your Pasta Bowl MEAT   128   Italian   Fall 2024
    ## 1534  2-Dec            Create Your Pasta Bowl VEG    24   Italian   Fall 2024
    ## 1535  2-Dec                          Pizza Cheese    32   Italian   Fall 2024
    ## 1536  2-Dec                   Pizza with Toppings    22   Italian   Fall 2024
    ## 1537  2-Dec                        Add Extra Meat    21   Italian   Fall 2024
    ## 1538  2-Dec              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 1539  2-Dec                     Burrito Breakfast    79 Breakfast   Fall 2024
    ## 1540  2-Dec                   Small French Omelet    44 Breakfast   Fall 2024
    ## 1541  2-Dec                  Grand Slam Breakfast    10 Breakfast   Fall 2024
    ## 1542  2-Dec      Egg Cheese Sausage Breakfast San    13 Breakfast   Fall 2024
    ## 1543  2-Dec      Egg Cheese Bacon Breakfast Sandw    11 Breakfast   Fall 2024
    ## 1544  2-Dec                             Add Bacon    15 Breakfast   Fall 2024
    ## 1545  2-Dec                              Two Eggs    13 Breakfast   Fall 2024
    ## 1546  2-Dec                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 1547  2-Dec                        Pancake Single     3 Breakfast   Fall 2024
    ## 1548  2-Dec                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 1549  2-Dec                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1550  2-Dec                      Burrito Bowl BYO    87   Mexican   Fall 2024
    ## 1551  2-Dec                           Single Taco     4   Mexican   Fall 2024
    ## 1552  2-Dec                        Side Guacamole     1   Mexican   Fall 2024
    ## 1553  2-Dec                            Soup 12 oz    56      Soup   Fall 2024
    ## 1554  2-Dec                             8 oz Soup    40      Soup   Fall 2024
    ## 1555  2-Dec                    Salad by the Pound    51 Salad Bar   Fall 2024
    ## 1556  2-Dec                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 1557  2-Dec                      Side Potato Tots    13 Grab N Go   Fall 2024
    ## 1558  3-Dec            Quesadilla Deluxe Trillium   184     Grill   Fall 2024
    ## 1559  3-Dec                     Grilled Hamburger    97     Grill   Fall 2024
    ## 1560  3-Dec                 Fried Chicken Tenders    97 Grab N Go   Fall 2024
    ## 1561  3-Dec         Burrito Una Mano Trillium BYO    64     Grill   Fall 2024
    ## 1562  3-Dec                          French Fries   134     Grill   Fall 2024
    ## 1563  3-Dec                     Quesadilla Cheese    17     Grill   Fall 2024
    ## 1564  3-Dec       Grilled Chicken Breast Sandwich    14     Grill   Fall 2024
    ## 1565  3-Dec      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 1566  3-Dec                    Sweet Potato Fries    26     Grill   Fall 2024
    ## 1567  3-Dec                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 1568  3-Dec                          + Beef Patty    15     Grill   Fall 2024
    ## 1569  3-Dec                     Black Bean Burger     4     Grill   Fall 2024
    ## 1570  3-Dec                    ADD Chicken Breast     9     Grill   Fall 2024
    ## 1571  3-Dec             ADD Burger Salmon Grilled     2     Grill   Fall 2024
    ## 1572  3-Dec                            ADD Cheese    13     Grill   Fall 2024
    ## 1573  3-Dec                           Add Egg .99     4     Grill   Fall 2024
    ## 1574  3-Dec                     1 Entree + 1 Side   199     Asian   Fall 2024
    ## 1575  3-Dec                     1 Entree + 2 Side    81     Asian   Fall 2024
    ## 1576  3-Dec                    Bowl Ramen Chicken    76     Asian   Fall 2024
    ## 1577  3-Dec                   2 Entrees + 2 Sides    22     Asian   Fall 2024
    ## 1578  3-Dec                       Bowl Ramen Tofu    21     Asian   Fall 2024
    ## 1579  3-Dec               Side Vegetarian Lo Mein     8     Asian   Fall 2024
    ## 1580  3-Dec           Side Vegetable Spring Rolls     4     Asian   Fall 2024
    ## 1581  3-Dec              Side White or Brown Rice     6     Asian   Fall 2024
    ## 1582  3-Dec       Side Vegetarian Fried Rice with     3     Asian   Fall 2024
    ## 1583  3-Dec                Side Fried Spring Roll     1     Asian   Fall 2024
    ## 1584  3-Dec                       Side Vegetables     1     Asian   Fall 2024
    ## 1585  3-Dec                    Add Extra Toppings     1     Asian   Fall 2024
    ## 1586  3-Dec           Create Your Pasta Bowl MEAT   116   Italian   Fall 2024
    ## 1587  3-Dec            Create Your Pasta Bowl VEG    28   Italian   Fall 2024
    ## 1588  3-Dec                   Pizza with Toppings    39   Italian   Fall 2024
    ## 1589  3-Dec                          Pizza Cheese    20   Italian   Fall 2024
    ## 1590  3-Dec                        Add Extra Meat    21   Italian   Fall 2024
    ## 1591  3-Dec                     Burrito Breakfast    72 Breakfast   Fall 2024
    ## 1592  3-Dec                   Small French Omelet    57 Breakfast   Fall 2024
    ## 1593  3-Dec      Egg Cheese Sausage Breakfast San    36 Breakfast   Fall 2024
    ## 1594  3-Dec      Egg Cheese Bacon Breakfast Sandw    33 Breakfast   Fall 2024
    ## 1595  3-Dec                  Grand Slam Breakfast    13 Breakfast   Fall 2024
    ## 1596  3-Dec                             Add Bacon    22 Breakfast   Fall 2024
    ## 1597  3-Dec                              Two Eggs    11 Breakfast   Fall 2024
    ## 1598  3-Dec                   Trillium Home Fries     2 Breakfast   Fall 2024
    ## 1599  3-Dec                        Pancake Single     2 Breakfast   Fall 2024
    ## 1600  3-Dec                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 1601  3-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1602  3-Dec                              PC Jelly     1 Breakfast   Fall 2024
    ## 1603  3-Dec                      Burrito Bowl BYO   115   Mexican   Fall 2024
    ## 1604  3-Dec                           Single Taco     4   Mexican   Fall 2024
    ## 1605  3-Dec                        Side Guacamole     6   Mexican   Fall 2024
    ## 1606  3-Dec           Add Extra Toppings Una Mano     3   Mexican   Fall 2024
    ## 1607  3-Dec                            Side Salsa     1   Mexican   Fall 2024
    ## 1608  3-Dec                            Soup 12 oz    53      Soup   Fall 2024
    ## 1609  3-Dec                             8 oz Soup    51      Soup   Fall 2024
    ## 1610  3-Dec                    Salad by the Pound    55 Salad Bar   Fall 2024
    ## 1611  3-Dec                Add Extra Protein 2.99     1 Salad Bar   Fall 2024
    ## 1612  3-Dec                      Side Potato Tots    17 Grab N Go   Fall 2024
    ## 1613  4-Dec            Quesadilla Deluxe Trillium   168     Grill   Fall 2024
    ## 1614  4-Dec                     Grilled Hamburger   117     Grill   Fall 2024
    ## 1615  4-Dec                 Fried Chicken Tenders    83 Grab N Go   Fall 2024
    ## 1616  4-Dec         Burrito Una Mano Trillium BYO    60     Grill   Fall 2024
    ## 1617  4-Dec                          French Fries   126     Grill   Fall 2024
    ## 1618  4-Dec       Grilled Chicken Breast Sandwich    16     Grill   Fall 2024
    ## 1619  4-Dec                     Quesadilla Cheese    17     Grill   Fall 2024
    ## 1620  4-Dec                    Sweet Potato Fries    28     Grill   Fall 2024
    ## 1621  4-Dec      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 1622  4-Dec                  Seared Salmon Burger     5     Grill   Fall 2024
    ## 1623  4-Dec                    ADD Chicken Breast    10     Grill   Fall 2024
    ## 1624  4-Dec                          + Beef Patty     7     Grill   Fall 2024
    ## 1625  4-Dec                   Add Sausage 2 Patty     4     Grill   Fall 2024
    ## 1626  4-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 1627  4-Dec                            ADD Cheese     5     Grill   Fall 2024
    ## 1628  4-Dec                           Add Egg .99     3     Grill   Fall 2024
    ## 1629  4-Dec                     1 Entree + 1 Side   202     Asian   Fall 2024
    ## 1630  4-Dec                     1 Entree + 2 Side    85     Asian   Fall 2024
    ## 1631  4-Dec                    Bowl Ramen Chicken    84     Asian   Fall 2024
    ## 1632  4-Dec                   2 Entrees + 2 Sides    35     Asian   Fall 2024
    ## 1633  4-Dec                       Bowl Ramen Tofu    13     Asian   Fall 2024
    ## 1634  4-Dec               Side Vegetarian Lo Mein    16     Asian   Fall 2024
    ## 1635  4-Dec                          1 Wok Entree     5     Asian   Fall 2024
    ## 1636  4-Dec              Side White or Brown Rice     9     Asian   Fall 2024
    ## 1637  4-Dec                       Side Vegetables     2     Asian   Fall 2024
    ## 1638  4-Dec                Side Fried Spring Roll     1     Asian   Fall 2024
    ## 1639  4-Dec       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1640  4-Dec           Create Your Pasta Bowl MEAT   142   Italian   Fall 2024
    ## 1641  4-Dec            Create Your Pasta Bowl VEG    27   Italian   Fall 2024
    ## 1642  4-Dec                   Pizza with Toppings    22   Italian   Fall 2024
    ## 1643  4-Dec                          Pizza Cheese    22   Italian   Fall 2024
    ## 1644  4-Dec                        Add Extra Meat    36   Italian   Fall 2024
    ## 1645  4-Dec              Side Bread Pasta Station     2   Italian   Fall 2024
    ## 1646  4-Dec                     Burrito Breakfast    91 Breakfast   Fall 2024
    ## 1647  4-Dec                   Small French Omelet    58 Breakfast   Fall 2024
    ## 1648  4-Dec                  Grand Slam Breakfast    19 Breakfast   Fall 2024
    ## 1649  4-Dec      Egg Cheese Sausage Breakfast San    30 Breakfast   Fall 2024
    ## 1650  4-Dec      Egg Cheese Bacon Breakfast Sandw    28 Breakfast   Fall 2024
    ## 1651  4-Dec                             Add Bacon    34 Breakfast   Fall 2024
    ## 1652  4-Dec                              Two Eggs    18 Breakfast   Fall 2024
    ## 1653  4-Dec                   Trillium Home Fries     5 Breakfast   Fall 2024
    ## 1654  4-Dec                        Pancake Single     2 Breakfast   Fall 2024
    ## 1655  4-Dec                        2 Slices Toast     4 Breakfast   Fall 2024
    ## 1656  4-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1657  4-Dec                      Burrito Bowl BYO    84   Mexican   Fall 2024
    ## 1658  4-Dec                           Single Taco    10   Mexican   Fall 2024
    ## 1659  4-Dec                        Side Guacamole     2   Mexican   Fall 2024
    ## 1660  4-Dec           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 1661  4-Dec                            Soup 12 oz    58      Soup   Fall 2024
    ## 1662  4-Dec                             8 oz Soup    59      Soup   Fall 2024
    ## 1663  4-Dec                    Salad by the Pound    61 Salad Bar   Fall 2024
    ## 1664  4-Dec                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 1665  4-Dec                      Side Potato Tots    11 Grab N Go   Fall 2024
    ## 1666  5-Dec            Quesadilla Deluxe Trillium   163     Grill   Fall 2024
    ## 1667  5-Dec                     Grilled Hamburger   118     Grill   Fall 2024
    ## 1668  5-Dec                 Fried Chicken Tenders   116 Grab N Go   Fall 2024
    ## 1669  5-Dec         Burrito Una Mano Trillium BYO    82     Grill   Fall 2024
    ## 1670  5-Dec                          French Fries   179     Grill   Fall 2024
    ## 1671  5-Dec       Grilled Chicken Breast Sandwich    22     Grill   Fall 2024
    ## 1672  5-Dec      Trillium Grill Impossible Burger    16     Grill   Fall 2024
    ## 1673  5-Dec                     Quesadilla Cheese    20     Grill   Fall 2024
    ## 1674  5-Dec                    Sweet Potato Fries    42     Grill   Fall 2024
    ## 1675  5-Dec                  Seared Salmon Burger    11     Grill   Fall 2024
    ## 1676  5-Dec                     Black Bean Burger     6     Grill   Fall 2024
    ## 1677  5-Dec                          + Beef Patty    15     Grill   Fall 2024
    ## 1678  5-Dec                    ADD Chicken Breast     4     Grill   Fall 2024
    ## 1679  5-Dec                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 1680  5-Dec                            ADD Cheese     9     Grill   Fall 2024
    ## 1681  5-Dec                           Add Egg .99     3     Grill   Fall 2024
    ## 1682  5-Dec                     1 Entree + 1 Side   212     Asian   Fall 2024
    ## 1683  5-Dec                     1 Entree + 2 Side    85     Asian   Fall 2024
    ## 1684  5-Dec                    Bowl Ramen Chicken    70     Asian   Fall 2024
    ## 1685  5-Dec                   2 Entrees + 2 Sides    41     Asian   Fall 2024
    ## 1686  5-Dec                       Bowl Ramen Tofu    22     Asian   Fall 2024
    ## 1687  5-Dec               Side Vegetarian Lo Mein    14     Asian   Fall 2024
    ## 1688  5-Dec                          1 Wok Entree     8     Asian   Fall 2024
    ## 1689  5-Dec              Side White or Brown Rice    10     Asian   Fall 2024
    ## 1690  5-Dec                Side Fried Spring Roll     2     Asian   Fall 2024
    ## 1691  5-Dec                       Side Vegetables     2     Asian   Fall 2024
    ## 1692  5-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1693  5-Dec           Create Your Pasta Bowl MEAT   113   Italian   Fall 2024
    ## 1694  5-Dec            Create Your Pasta Bowl VEG    32   Italian   Fall 2024
    ## 1695  5-Dec                   Pizza with Toppings    37   Italian   Fall 2024
    ## 1696  5-Dec                          Pizza Cheese    23   Italian   Fall 2024
    ## 1697  5-Dec                        Add Extra Meat    23   Italian   Fall 2024
    ## 1698  5-Dec              Side Bread Pasta Station     1   Italian   Fall 2024
    ## 1699  5-Dec                     Burrito Breakfast    69 Breakfast   Fall 2024
    ## 1700  5-Dec                   Small French Omelet    36 Breakfast   Fall 2024
    ## 1701  5-Dec                  Grand Slam Breakfast    18 Breakfast   Fall 2024
    ## 1702  5-Dec      Egg Cheese Sausage Breakfast San    24 Breakfast   Fall 2024
    ## 1703  5-Dec      Egg Cheese Bacon Breakfast Sandw    20 Breakfast   Fall 2024
    ## 1704  5-Dec                             Add Bacon    28 Breakfast   Fall 2024
    ## 1705  5-Dec                              Two Eggs    15 Breakfast   Fall 2024
    ## 1706  5-Dec                   Trillium Home Fries     2 Breakfast   Fall 2024
    ## 1707  5-Dec                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 1708  5-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1709  5-Dec                      Burrito Bowl BYO    93   Mexican   Fall 2024
    ## 1710  5-Dec                           Single Taco     4   Mexican   Fall 2024
    ## 1711  5-Dec                        Side Guacamole     2   Mexican   Fall 2024
    ## 1712  5-Dec           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 1713  5-Dec                             8 oz Soup    66      Soup   Fall 2024
    ## 1714  5-Dec                            Soup 12 oz    36      Soup   Fall 2024
    ## 1715  5-Dec                    Salad by the Pound    47 Salad Bar   Fall 2024
    ## 1716  5-Dec                Add Extra Protein 2.99     2 Salad Bar   Fall 2024
    ## 1717  5-Dec                      Side Potato Tots    11 Grab N Go   Fall 2024
    ## 1718  6-Dec            Quesadilla Deluxe Trillium   114     Grill   Fall 2024
    ## 1719  6-Dec                     Grilled Hamburger    76     Grill   Fall 2024
    ## 1720  6-Dec         Burrito Una Mano Trillium BYO    50     Grill   Fall 2024
    ## 1721  6-Dec                 Fried Chicken Tenders    63 Grab N Go   Fall 2024
    ## 1722  6-Dec                          French Fries   113     Grill   Fall 2024
    ## 1723  6-Dec                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 1724  6-Dec      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 1725  6-Dec       Grilled Chicken Breast Sandwich     8     Grill   Fall 2024
    ## 1726  6-Dec                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 1727  6-Dec                     Black Bean Burger     5     Grill   Fall 2024
    ## 1728  6-Dec                    Sweet Potato Fries     7     Grill   Fall 2024
    ## 1729  6-Dec                   Add Sausage 2 Patty     4     Grill   Fall 2024
    ## 1730  6-Dec                          + Beef Patty     2     Grill   Fall 2024
    ## 1731  6-Dec                            ADD Cheese     5     Grill   Fall 2024
    ## 1732  6-Dec                           Add Egg .99     2     Grill   Fall 2024
    ## 1733  6-Dec                     1 Entree + 1 Side   137     Asian   Fall 2024
    ## 1734  6-Dec                     1 Entree + 2 Side    61     Asian   Fall 2024
    ## 1735  6-Dec                    Bowl Ramen Chicken    58     Asian   Fall 2024
    ## 1736  6-Dec                   2 Entrees + 2 Sides    23     Asian   Fall 2024
    ## 1737  6-Dec                       Bowl Ramen Tofu    18     Asian   Fall 2024
    ## 1738  6-Dec                          1 Wok Entree     5     Asian   Fall 2024
    ## 1739  6-Dec               Side Vegetarian Lo Mein     5     Asian   Fall 2024
    ## 1740  6-Dec                       Side Vegetables     1     Asian   Fall 2024
    ## 1741  6-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1742  6-Dec       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 1743  6-Dec              Side White or Brown Rice     1     Asian   Fall 2024
    ## 1744  6-Dec                     Burrito Breakfast    72 Breakfast   Fall 2024
    ## 1745  6-Dec                   Small French Omelet    49 Breakfast   Fall 2024
    ## 1746  6-Dec                  Grand Slam Breakfast    15 Breakfast   Fall 2024
    ## 1747  6-Dec      Egg Cheese Sausage Breakfast San    26 Breakfast   Fall 2024
    ## 1748  6-Dec      Egg Cheese Bacon Breakfast Sandw    20 Breakfast   Fall 2024
    ## 1749  6-Dec                             Add Bacon    27 Breakfast   Fall 2024
    ## 1750  6-Dec                              Two Eggs    18 Breakfast   Fall 2024
    ## 1751  6-Dec                        Pancake Single     4 Breakfast   Fall 2024
    ## 1752  6-Dec                   Trillium Home Fries     3 Breakfast   Fall 2024
    ## 1753  6-Dec                        2 Slices Toast     5 Breakfast   Fall 2024
    ## 1754  6-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1755  6-Dec                      PC Peanut Butter     1 Breakfast   Fall 2024
    ## 1756  6-Dec           Create Your Pasta Bowl MEAT    83   Italian   Fall 2024
    ## 1757  6-Dec                   Pizza with Toppings    28   Italian   Fall 2024
    ## 1758  6-Dec            Create Your Pasta Bowl VEG    12   Italian   Fall 2024
    ## 1759  6-Dec                          Pizza Cheese    19   Italian   Fall 2024
    ## 1760  6-Dec                        Add Extra Meat    26   Italian   Fall 2024
    ## 1761  6-Dec                      Burrito Bowl BYO    66   Mexican   Fall 2024
    ## 1762  6-Dec                           Single Taco     6   Mexican   Fall 2024
    ## 1763  6-Dec                            Soup 12 oz    34      Soup   Fall 2024
    ## 1764  6-Dec                             8 oz Soup    30      Soup   Fall 2024
    ## 1765  6-Dec                    Salad by the Pound    34 Salad Bar   Fall 2024
    ## 1766  6-Dec                      Side Potato Tots    21 Grab N Go   Fall 2024
    ## 1767  9-Dec            Quesadilla Deluxe Trillium   170     Grill   Fall 2024
    ## 1768  9-Dec                     Grilled Hamburger    96     Grill   Fall 2024
    ## 1769  9-Dec                 Fried Chicken Tenders    78 Grab N Go   Fall 2024
    ## 1770  9-Dec         Burrito Una Mano Trillium BYO    54     Grill   Fall 2024
    ## 1771  9-Dec                          French Fries   116     Grill   Fall 2024
    ## 1772  9-Dec                  Seared Salmon Burger    12     Grill   Fall 2024
    ## 1773  9-Dec      Trillium Grill Impossible Burger     9     Grill   Fall 2024
    ## 1774  9-Dec       Grilled Chicken Breast Sandwich     8     Grill   Fall 2024
    ## 1775  9-Dec                    Sweet Potato Fries    22     Grill   Fall 2024
    ## 1776  9-Dec                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 1777  9-Dec                     Black Bean Burger     4     Grill   Fall 2024
    ## 1778  9-Dec                          + Beef Patty     8     Grill   Fall 2024
    ## 1779  9-Dec                    ADD Chicken Breast     6     Grill   Fall 2024
    ## 1780  9-Dec                            ADD Cheese     9     Grill   Fall 2024
    ## 1781  9-Dec                           Add Egg .99     4     Grill   Fall 2024
    ## 1782  9-Dec                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 1783  9-Dec                     1 Entree + 1 Side   165     Asian   Fall 2024
    ## 1784  9-Dec                     1 Entree + 2 Side    59     Asian   Fall 2024
    ## 1785  9-Dec                    Bowl Ramen Chicken    57     Asian   Fall 2024
    ## 1786  9-Dec                   2 Entrees + 2 Sides    17     Asian   Fall 2024
    ## 1787  9-Dec                       Bowl Ramen Tofu    20     Asian   Fall 2024
    ## 1788  9-Dec               Side Vegetarian Lo Mein    11     Asian   Fall 2024
    ## 1789  9-Dec                          1 Wok Entree     3     Asian   Fall 2024
    ## 1790  9-Dec              Side White or Brown Rice     7     Asian   Fall 2024
    ## 1791  9-Dec           Side Vegetable Spring Rolls     3     Asian   Fall 2024
    ## 1792  9-Dec           Create Your Pasta Bowl MEAT   108   Italian   Fall 2024
    ## 1793  9-Dec            Create Your Pasta Bowl VEG    32   Italian   Fall 2024
    ## 1794  9-Dec                   Pizza with Toppings    28   Italian   Fall 2024
    ## 1795  9-Dec                          Pizza Cheese    17   Italian   Fall 2024
    ## 1796  9-Dec                        Add Extra Meat    28   Italian   Fall 2024
    ## 1797  9-Dec                     Burrito Breakfast    70 Breakfast   Fall 2024
    ## 1798  9-Dec                   Small French Omelet    44 Breakfast   Fall 2024
    ## 1799  9-Dec      Egg Cheese Sausage Breakfast San    25 Breakfast   Fall 2024
    ## 1800  9-Dec                  Grand Slam Breakfast    12 Breakfast   Fall 2024
    ## 1801  9-Dec      Egg Cheese Bacon Breakfast Sandw    21 Breakfast   Fall 2024
    ## 1802  9-Dec                             Add Bacon    32 Breakfast   Fall 2024
    ## 1803  9-Dec                              Two Eggs    13 Breakfast   Fall 2024
    ## 1804  9-Dec                        Pancake Single     6 Breakfast   Fall 2024
    ## 1805  9-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1806  9-Dec                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 1807  9-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1808  9-Dec                      Burrito Bowl BYO    93   Mexican   Fall 2024
    ## 1809  9-Dec                           Single Taco     6   Mexican   Fall 2024
    ## 1810  9-Dec                            Soup 12 oz    47      Soup   Fall 2024
    ## 1811  9-Dec                             8 oz Soup    32      Soup   Fall 2024
    ## 1812  9-Dec                    Salad by the Pound    34 Salad Bar   Fall 2024
    ## 1813  9-Dec                      Side Potato Tots    17 Grab N Go   Fall 2024
    ## 1814 10-Dec            Quesadilla Deluxe Trillium   133     Grill   Fall 2024
    ## 1815 10-Dec                     Grilled Hamburger    52     Grill   Fall 2024
    ## 1816 10-Dec         Burrito Una Mano Trillium BYO    30     Grill   Fall 2024
    ## 1817 10-Dec                 Fried Chicken Tenders    34 Grab N Go   Fall 2024
    ## 1818 10-Dec                          French Fries    65     Grill   Fall 2024
    ## 1819 10-Dec      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 1820 10-Dec                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 1821 10-Dec       Grilled Chicken Breast Sandwich     6     Grill   Fall 2024
    ## 1822 10-Dec                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 1823 10-Dec                    Sweet Potato Fries    12     Grill   Fall 2024
    ## 1824 10-Dec                          + Beef Patty     4     Grill   Fall 2024
    ## 1825 10-Dec                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 1826 10-Dec                           Add Egg .99     1     Grill   Fall 2024
    ## 1827 10-Dec                            ADD Cheese     1     Grill   Fall 2024
    ## 1828 10-Dec                     1 Entree + 1 Side    76     Asian   Fall 2024
    ## 1829 10-Dec                    Bowl Ramen Chicken    37     Asian   Fall 2024
    ## 1830 10-Dec                     1 Entree + 2 Side    32     Asian   Fall 2024
    ## 1831 10-Dec                   2 Entrees + 2 Sides    15     Asian   Fall 2024
    ## 1832 10-Dec                       Bowl Ramen Tofu     4     Asian   Fall 2024
    ## 1833 10-Dec                       Side Vegetables     2     Asian   Fall 2024
    ## 1834 10-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1835 10-Dec               Side Vegetarian Lo Mein     1     Asian   Fall 2024
    ## 1836 10-Dec              Side White or Brown Rice     1     Asian   Fall 2024
    ## 1837 10-Dec           Create Your Pasta Bowl MEAT    45   Italian   Fall 2024
    ## 1838 10-Dec                   Pizza with Toppings    22   Italian   Fall 2024
    ## 1839 10-Dec            Create Your Pasta Bowl VEG     9   Italian   Fall 2024
    ## 1840 10-Dec                          Pizza Cheese    11   Italian   Fall 2024
    ## 1841 10-Dec                        Add Extra Meat    12   Italian   Fall 2024
    ## 1842 10-Dec                     Burrito Breakfast    36 Breakfast   Fall 2024
    ## 1843 10-Dec                   Small French Omelet    22 Breakfast   Fall 2024
    ## 1844 10-Dec                  Grand Slam Breakfast     8 Breakfast   Fall 2024
    ## 1845 10-Dec                             Add Bacon    13 Breakfast   Fall 2024
    ## 1846 10-Dec                        Pancake Single     3 Breakfast   Fall 2024
    ## 1847 10-Dec      Egg Cheese Sausage Breakfast San     1 Breakfast   Fall 2024
    ## 1848 10-Dec                              Two Eggs     2 Breakfast   Fall 2024
    ## 1849 10-Dec                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 1850 10-Dec                      Burrito Bowl BYO    40   Mexican   Fall 2024
    ## 1851 10-Dec                           Single Taco     8   Mexican   Fall 2024
    ## 1852 10-Dec                        Side Guacamole     1   Mexican   Fall 2024
    ## 1853 10-Dec           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 1854 10-Dec                    Salad by the Pound    32 Salad Bar   Fall 2024
    ## 1855 10-Dec                             8 oz Soup    24      Soup   Fall 2024
    ## 1856 10-Dec                            Soup 12 oz    18      Soup   Fall 2024
    ## 1857 10-Dec                      Side Potato Tots     6 Grab N Go   Fall 2024
    ## 1858 11-Dec            Quesadilla Deluxe Trillium   111     Grill   Fall 2024
    ## 1859 11-Dec                     Grilled Hamburger    47     Grill   Fall 2024
    ## 1860 11-Dec         Burrito Una Mano Trillium BYO    38     Grill   Fall 2024
    ## 1861 11-Dec                          French Fries    71     Grill   Fall 2024
    ## 1862 11-Dec                 Fried Chicken Tenders    28 Grab N Go   Fall 2024
    ## 1863 11-Dec                     Quesadilla Cheese    11     Grill   Fall 2024
    ## 1864 11-Dec       Grilled Chicken Breast Sandwich     8     Grill   Fall 2024
    ## 1865 11-Dec      Trillium Grill Impossible Burger     5     Grill   Fall 2024
    ## 1866 11-Dec                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 1867 11-Dec                     Black Bean Burger     2     Grill   Fall 2024
    ## 1868 11-Dec                          + Beef Patty     4     Grill   Fall 2024
    ## 1869 11-Dec             ADD Burger Salmon Grilled     2     Grill   Fall 2024
    ## 1870 11-Dec                           Add Egg .99     4     Grill   Fall 2024
    ## 1871 11-Dec                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 1872 11-Dec                            ADD Cheese     3     Grill   Fall 2024
    ## 1873 11-Dec                     1 Entree + 1 Side    70     Asian   Fall 2024
    ## 1874 11-Dec                    Bowl Ramen Chicken    42     Asian   Fall 2024
    ## 1875 11-Dec                     1 Entree + 2 Side    31     Asian   Fall 2024
    ## 1876 11-Dec                   2 Entrees + 2 Sides    15     Asian   Fall 2024
    ## 1877 11-Dec                       Bowl Ramen Tofu     7     Asian   Fall 2024
    ## 1878 11-Dec                          1 Wok Entree     4     Asian   Fall 2024
    ## 1879 11-Dec                       Side Vegetables     5     Asian   Fall 2024
    ## 1880 11-Dec               Side Vegetarian Lo Mein     3     Asian   Fall 2024
    ## 1881 11-Dec              Side White or Brown Rice     5     Asian   Fall 2024
    ## 1882 11-Dec       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1883 11-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1884 11-Dec           Create Your Pasta Bowl MEAT    41   Italian   Fall 2024
    ## 1885 11-Dec            Create Your Pasta Bowl VEG    15   Italian   Fall 2024
    ## 1886 11-Dec                   Pizza with Toppings    17   Italian   Fall 2024
    ## 1887 11-Dec                          Pizza Cheese     6   Italian   Fall 2024
    ## 1888 11-Dec                        Add Extra Meat     5   Italian   Fall 2024
    ## 1889 11-Dec                     Burrito Breakfast    37 Breakfast   Fall 2024
    ## 1890 11-Dec                   Small French Omelet    24 Breakfast   Fall 2024
    ## 1891 11-Dec      Egg Cheese Sausage Breakfast San     9 Breakfast   Fall 2024
    ## 1892 11-Dec                  Grand Slam Breakfast     4 Breakfast   Fall 2024
    ## 1893 11-Dec                             Add Bacon     6 Breakfast   Fall 2024
    ## 1894 11-Dec      Egg Cheese Bacon Breakfast Sandw     2 Breakfast   Fall 2024
    ## 1895 11-Dec                        Pancake Single     4 Breakfast   Fall 2024
    ## 1896 11-Dec                              Two Eggs     3 Breakfast   Fall 2024
    ## 1897 11-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1898 11-Dec                      Burrito Bowl BYO    49   Mexican   Fall 2024
    ## 1899 11-Dec                           Single Taco     2   Mexican   Fall 2024
    ## 1900 11-Dec                        Side Guacamole     1   Mexican   Fall 2024
    ## 1901 11-Dec                    Salad by the Pound    30 Salad Bar   Fall 2024
    ## 1902 11-Dec                            Soup 12 oz    20      Soup   Fall 2024
    ## 1903 11-Dec                             8 oz Soup    18      Soup   Fall 2024
    ## 1904 11-Dec                      Side Potato Tots    11 Grab N Go   Fall 2024
    ## 1905 12-Dec            Quesadilla Deluxe Trillium   144     Grill   Fall 2024
    ## 1906 12-Dec                     Grilled Hamburger    69     Grill   Fall 2024
    ## 1907 12-Dec         Burrito Una Mano Trillium BYO    33     Grill   Fall 2024
    ## 1908 12-Dec                 Fried Chicken Tenders    39 Grab N Go   Fall 2024
    ## 1909 12-Dec                          French Fries    73     Grill   Fall 2024
    ## 1910 12-Dec       Grilled Chicken Breast Sandwich    13     Grill   Fall 2024
    ## 1911 12-Dec      Trillium Grill Impossible Burger     8     Grill   Fall 2024
    ## 1912 12-Dec                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 1913 12-Dec                     Quesadilla Cheese     8     Grill   Fall 2024
    ## 1914 12-Dec                    Sweet Potato Fries    15     Grill   Fall 2024
    ## 1915 12-Dec                          + Beef Patty    12     Grill   Fall 2024
    ## 1916 12-Dec                    ADD Chicken Breast     5     Grill   Fall 2024
    ## 1917 12-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 1918 12-Dec                            ADD Cheese    10     Grill   Fall 2024
    ## 1919 12-Dec             ADD Burger Salmon Grilled     1     Grill   Fall 2024
    ## 1920 12-Dec                           Add Egg .99     2     Grill   Fall 2024
    ## 1921 12-Dec                     1 Entree + 1 Side    83     Asian   Fall 2024
    ## 1922 12-Dec                     1 Entree + 2 Side    44     Asian   Fall 2024
    ## 1923 12-Dec                    Bowl Ramen Chicken    46     Asian   Fall 2024
    ## 1924 12-Dec                   2 Entrees + 2 Sides     9     Asian   Fall 2024
    ## 1925 12-Dec                       Bowl Ramen Tofu     9     Asian   Fall 2024
    ## 1926 12-Dec               Side Vegetarian Lo Mein     7     Asian   Fall 2024
    ## 1927 12-Dec                          1 Wok Entree     4     Asian   Fall 2024
    ## 1928 12-Dec              Side White or Brown Rice     6     Asian   Fall 2024
    ## 1929 12-Dec       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1930 12-Dec                Side Fried Spring Roll     1     Asian   Fall 2024
    ## 1931 12-Dec                       Side Vegetables     1     Asian   Fall 2024
    ## 1932 12-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1933 12-Dec                   Small French Omelet    34 Breakfast   Fall 2024
    ## 1934 12-Dec                     Burrito Breakfast    36 Breakfast   Fall 2024
    ## 1935 12-Dec                  Grand Slam Breakfast     7 Breakfast   Fall 2024
    ## 1936 12-Dec      Egg Cheese Sausage Breakfast San    11 Breakfast   Fall 2024
    ## 1937 12-Dec                             Add Bacon    24 Breakfast   Fall 2024
    ## 1938 12-Dec      Egg Cheese Bacon Breakfast Sandw     8 Breakfast   Fall 2024
    ## 1939 12-Dec                              Two Eggs     6 Breakfast   Fall 2024
    ## 1940 12-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1941 12-Dec                        Pancake Single     1 Breakfast   Fall 2024
    ## 1942 12-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 1943 12-Dec                              PC Jelly     1 Breakfast   Fall 2024
    ## 1944 12-Dec           Create Your Pasta Bowl MEAT    34   Italian   Fall 2024
    ## 1945 12-Dec            Create Your Pasta Bowl VEG    14   Italian   Fall 2024
    ## 1946 12-Dec                   Pizza with Toppings    19   Italian   Fall 2024
    ## 1947 12-Dec                          Pizza Cheese     9   Italian   Fall 2024
    ## 1948 12-Dec                        Add Extra Meat     9   Italian   Fall 2024
    ## 1949 12-Dec                      Burrito Bowl BYO    55   Mexican   Fall 2024
    ## 1950 12-Dec                           Single Taco    11   Mexican   Fall 2024
    ## 1951 12-Dec                        Side Guacamole     3   Mexican   Fall 2024
    ## 1952 12-Dec           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 1953 12-Dec                    Salad by the Pound    38 Salad Bar   Fall 2024
    ## 1954 12-Dec                             8 oz Soup    30      Soup   Fall 2024
    ## 1955 12-Dec                            Soup 12 oz    18      Soup   Fall 2024
    ## 1956 12-Dec                      Side Potato Tots     7 Grab N Go   Fall 2024
    ## 1957 13-Dec            Quesadilla Deluxe Trillium   118     Grill   Fall 2024
    ## 1958 13-Dec                     Grilled Hamburger    49     Grill   Fall 2024
    ## 1959 13-Dec         Burrito Una Mano Trillium BYO    43     Grill   Fall 2024
    ## 1960 13-Dec                 Fried Chicken Tenders    34 Grab N Go   Fall 2024
    ## 1961 13-Dec                          French Fries    84     Grill   Fall 2024
    ## 1962 13-Dec       Grilled Chicken Breast Sandwich    13     Grill   Fall 2024
    ## 1963 13-Dec                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 1964 13-Dec                          + Beef Patty     9     Grill   Fall 2024
    ## 1965 13-Dec                  Seared Salmon Burger     2     Grill   Fall 2024
    ## 1966 13-Dec      Trillium Grill Impossible Burger     1     Grill   Fall 2024
    ## 1967 13-Dec                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 1968 13-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 1969 13-Dec                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 1970 13-Dec                            ADD Cheese     5     Grill   Fall 2024
    ## 1971 13-Dec                           Add Egg .99     3     Grill   Fall 2024
    ## 1972 13-Dec                     1 Entree + 1 Side    87     Asian   Fall 2024
    ## 1973 13-Dec                    Bowl Ramen Chicken    50     Asian   Fall 2024
    ## 1974 13-Dec                     1 Entree + 2 Side    44     Asian   Fall 2024
    ## 1975 13-Dec                   2 Entrees + 2 Sides     9     Asian   Fall 2024
    ## 1976 13-Dec                       Bowl Ramen Tofu     7     Asian   Fall 2024
    ## 1977 13-Dec               Side Vegetarian Lo Mein     5     Asian   Fall 2024
    ## 1978 13-Dec              Side White or Brown Rice     6     Asian   Fall 2024
    ## 1979 13-Dec       Side Vegetarian Fried Rice with     2     Asian   Fall 2024
    ## 1980 13-Dec                          1 Wok Entree     1     Asian   Fall 2024
    ## 1981 13-Dec                       Side Vegetables     1     Asian   Fall 2024
    ## 1982 13-Dec           Side Vegetable Spring Rolls     1     Asian   Fall 2024
    ## 1983 13-Dec                     Burrito Breakfast    44 Breakfast   Fall 2024
    ## 1984 13-Dec                   Small French Omelet    32 Breakfast   Fall 2024
    ## 1985 13-Dec                  Grand Slam Breakfast     7 Breakfast   Fall 2024
    ## 1986 13-Dec                             Add Bacon    17 Breakfast   Fall 2024
    ## 1987 13-Dec                              Two Eggs     6 Breakfast   Fall 2024
    ## 1988 13-Dec                        Pancake Single     3 Breakfast   Fall 2024
    ## 1989 13-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 1990 13-Dec                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 1991 13-Dec           Create Your Pasta Bowl MEAT    45   Italian   Fall 2024
    ## 1992 13-Dec                   Pizza with Toppings    13   Italian   Fall 2024
    ## 1993 13-Dec                          Pizza Cheese    10   Italian   Fall 2024
    ## 1994 13-Dec            Create Your Pasta Bowl VEG     5   Italian   Fall 2024
    ## 1995 13-Dec                        Add Extra Meat     5   Italian   Fall 2024
    ## 1996 13-Dec                      Burrito Bowl BYO    32   Mexican   Fall 2024
    ## 1997 13-Dec                           Single Taco     3   Mexican   Fall 2024
    ## 1998 13-Dec                        Side Guacamole     1   Mexican   Fall 2024
    ## 1999 13-Dec           Add Extra Toppings Una Mano     2   Mexican   Fall 2024
    ## 2000 13-Dec                             8 oz Soup    28      Soup   Fall 2024
    ## 2001 13-Dec                            Soup 12 oz    22      Soup   Fall 2024
    ## 2002 13-Dec                    Salad by the Pound    25 Salad Bar   Fall 2024
    ## 2003 13-Dec                      Side Potato Tots     3 Grab N Go   Fall 2024
    ## 2004 16-Dec            Quesadilla Deluxe Trillium   114     Grill   Fall 2024
    ## 2005 16-Dec                     Grilled Hamburger    55     Grill   Fall 2024
    ## 2006 16-Dec                 Fried Chicken Tenders    41 Grab N Go   Fall 2024
    ## 2007 16-Dec         Burrito Una Mano Trillium BYO    26     Grill   Fall 2024
    ## 2008 16-Dec                          French Fries    71     Grill   Fall 2024
    ## 2009 16-Dec                     Quesadilla Cheese    14     Grill   Fall 2024
    ## 2010 16-Dec                  Seared Salmon Burger     6     Grill   Fall 2024
    ## 2011 16-Dec      Trillium Grill Impossible Burger     4     Grill   Fall 2024
    ## 2012 16-Dec       Grilled Chicken Breast Sandwich     4     Grill   Fall 2024
    ## 2013 16-Dec                    Sweet Potato Fries    10     Grill   Fall 2024
    ## 2014 16-Dec                          + Beef Patty     6     Grill   Fall 2024
    ## 2015 16-Dec                    ADD Chicken Breast     3     Grill   Fall 2024
    ## 2016 16-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 2017 16-Dec                   Add Sausage 2 Patty     2     Grill   Fall 2024
    ## 2018 16-Dec             ADD Burger Salmon Grilled     1     Grill   Fall 2024
    ## 2019 16-Dec                           Add Egg .99     4     Grill   Fall 2024
    ## 2020 16-Dec                            ADD Cheese     5     Grill   Fall 2024
    ## 2021 16-Dec                     1 Entree + 1 Side    65     Asian   Fall 2024
    ## 2022 16-Dec                    Bowl Ramen Chicken    40     Asian   Fall 2024
    ## 2023 16-Dec                     1 Entree + 2 Side    33     Asian   Fall 2024
    ## 2024 16-Dec                   2 Entrees + 2 Sides     9     Asian   Fall 2024
    ## 2025 16-Dec                       Bowl Ramen Tofu     7     Asian   Fall 2024
    ## 2026 16-Dec                          1 Wok Entree     5     Asian   Fall 2024
    ## 2027 16-Dec               Side Vegetarian Lo Mein     6     Asian   Fall 2024
    ## 2028 16-Dec              Side White or Brown Rice     8     Asian   Fall 2024
    ## 2029 16-Dec                       Side Vegetables     2     Asian   Fall 2024
    ## 2030 16-Dec       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 2031 16-Dec                     Burrito Breakfast    47 Breakfast   Fall 2024
    ## 2032 16-Dec                   Small French Omelet    23 Breakfast   Fall 2024
    ## 2033 16-Dec                  Grand Slam Breakfast     8 Breakfast   Fall 2024
    ## 2034 16-Dec                             Add Bacon    18 Breakfast   Fall 2024
    ## 2035 16-Dec                              Two Eggs    11 Breakfast   Fall 2024
    ## 2036 16-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 2037 16-Dec                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 2038 16-Dec           Create Your Pasta Bowl MEAT    46   Italian   Fall 2024
    ## 2039 16-Dec                   Pizza with Toppings    16   Italian   Fall 2024
    ## 2040 16-Dec            Create Your Pasta Bowl VEG     7   Italian   Fall 2024
    ## 2041 16-Dec                          Pizza Cheese     8   Italian   Fall 2024
    ## 2042 16-Dec                        Add Extra Meat     9   Italian   Fall 2024
    ## 2043 16-Dec                      Burrito Bowl BYO    32   Mexican   Fall 2024
    ## 2044 16-Dec                    Salad by the Pound    26 Salad Bar   Fall 2024
    ## 2045 16-Dec                             8 oz Soup    18      Soup   Fall 2024
    ## 2046 16-Dec                            Soup 12 oz    15      Soup   Fall 2024
    ## 2047 16-Dec                      Side Potato Tots    10 Grab N Go   Fall 2024
    ## 2048 17-Dec            Quesadilla Deluxe Trillium   104     Grill   Fall 2024
    ## 2049 17-Dec                     Grilled Hamburger    61     Grill   Fall 2024
    ## 2050 17-Dec         Burrito Una Mano Trillium BYO    23     Grill   Fall 2024
    ## 2051 17-Dec                          French Fries    64     Grill   Fall 2024
    ## 2052 17-Dec                 Fried Chicken Tenders    25 Grab N Go   Fall 2024
    ## 2053 17-Dec                     Quesadilla Cheese    12     Grill   Fall 2024
    ## 2054 17-Dec       Grilled Chicken Breast Sandwich    10     Grill   Fall 2024
    ## 2055 17-Dec      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 2056 17-Dec                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 2057 17-Dec                    Sweet Potato Fries    13     Grill   Fall 2024
    ## 2058 17-Dec                     Black Bean Burger     3     Grill   Fall 2024
    ## 2059 17-Dec                          + Beef Patty     5     Grill   Fall 2024
    ## 2060 17-Dec                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 2061 17-Dec                            ADD Cheese     5     Grill   Fall 2024
    ## 2062 17-Dec                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 2063 17-Dec                     1 Entree + 1 Side    62     Asian   Fall 2024
    ## 2064 17-Dec                     1 Entree + 2 Side    47     Asian   Fall 2024
    ## 2065 17-Dec                    Bowl Ramen Chicken    35     Asian   Fall 2024
    ## 2066 17-Dec                   2 Entrees + 2 Sides    10     Asian   Fall 2024
    ## 2067 17-Dec                       Bowl Ramen Tofu     6     Asian   Fall 2024
    ## 2068 17-Dec                          1 Wok Entree     7     Asian   Fall 2024
    ## 2069 17-Dec               Side Vegetarian Lo Mein     3     Asian   Fall 2024
    ## 2070 17-Dec              Side White or Brown Rice     5     Asian   Fall 2024
    ## 2071 17-Dec           Side Vegetable Spring Rolls     2     Asian   Fall 2024
    ## 2072 17-Dec                     Burrito Breakfast    39 Breakfast   Fall 2024
    ## 2073 17-Dec                   Small French Omelet    18 Breakfast   Fall 2024
    ## 2074 17-Dec                  Grand Slam Breakfast     4 Breakfast   Fall 2024
    ## 2075 17-Dec                             Add Bacon    15 Breakfast   Fall 2024
    ## 2076 17-Dec                              Two Eggs     6 Breakfast   Fall 2024
    ## 2077 17-Dec                        2 Slices Toast     2 Breakfast   Fall 2024
    ## 2078 17-Dec           Create Your Pasta Bowl MEAT    30   Italian   Fall 2024
    ## 2079 17-Dec                   Pizza with Toppings    16   Italian   Fall 2024
    ## 2080 17-Dec            Create Your Pasta Bowl VEG    10   Italian   Fall 2024
    ## 2081 17-Dec                          Pizza Cheese    12   Italian   Fall 2024
    ## 2082 17-Dec                        Add Extra Meat     2   Italian   Fall 2024
    ## 2083 17-Dec                      Burrito Bowl BYO    45   Mexican   Fall 2024
    ## 2084 17-Dec                        Side Guacamole     2   Mexican   Fall 2024
    ## 2085 17-Dec                            Soup 12 oz    25      Soup   Fall 2024
    ## 2086 17-Dec                             8 oz Soup    18      Soup   Fall 2024
    ## 2087 17-Dec                    Salad by the Pound    24 Salad Bar   Fall 2024
    ## 2088 17-Dec                      Side Potato Tots     4 Grab N Go   Fall 2024
    ## 2089 18-Dec            Quesadilla Deluxe Trillium    83     Grill   Fall 2024
    ## 2090 18-Dec                     Grilled Hamburger    47     Grill   Fall 2024
    ## 2091 18-Dec         Burrito Una Mano Trillium BYO    21     Grill   Fall 2024
    ## 2092 18-Dec                 Fried Chicken Tenders    25 Grab N Go   Fall 2024
    ## 2093 18-Dec                          French Fries    61     Grill   Fall 2024
    ## 2094 18-Dec       Grilled Chicken Breast Sandwich    10     Grill   Fall 2024
    ## 2095 18-Dec      Trillium Grill Impossible Burger     6     Grill   Fall 2024
    ## 2096 18-Dec                  Seared Salmon Burger     7     Grill   Fall 2024
    ## 2097 18-Dec                     Quesadilla Cheese     7     Grill   Fall 2024
    ## 2098 18-Dec                    Sweet Potato Fries    13     Grill   Fall 2024
    ## 2099 18-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 2100 18-Dec                    ADD Chicken Breast     2     Grill   Fall 2024
    ## 2101 18-Dec                          + Beef Patty     2     Grill   Fall 2024
    ## 2102 18-Dec                           Add Egg .99     4     Grill   Fall 2024
    ## 2103 18-Dec                   Add Sausage 2 Patty     1     Grill   Fall 2024
    ## 2104 18-Dec                            ADD Cheese     2     Grill   Fall 2024
    ## 2105 18-Dec                     1 Entree + 1 Side    57     Asian   Fall 2024
    ## 2106 18-Dec                    Bowl Ramen Chicken    38     Asian   Fall 2024
    ## 2107 18-Dec                     1 Entree + 2 Side    30     Asian   Fall 2024
    ## 2108 18-Dec                   2 Entrees + 2 Sides     9     Asian   Fall 2024
    ## 2109 18-Dec                       Bowl Ramen Tofu     7     Asian   Fall 2024
    ## 2110 18-Dec              Side White or Brown Rice     5     Asian   Fall 2024
    ## 2111 18-Dec                    Bowl Ramen Chicken     1     Asian   Fall 2024
    ## 2112 18-Dec               Side Vegetarian Lo Mein     2     Asian   Fall 2024
    ## 2113 18-Dec       Side Vegetarian Fried Rice with     1     Asian   Fall 2024
    ## 2114 18-Dec                     Burrito Breakfast    38 Breakfast   Fall 2024
    ## 2115 18-Dec                   Small French Omelet    14 Breakfast   Fall 2024
    ## 2116 18-Dec                  Grand Slam Breakfast     8 Breakfast   Fall 2024
    ## 2117 18-Dec                             Add Bacon    13 Breakfast   Fall 2024
    ## 2118 18-Dec                              Two Eggs     6 Breakfast   Fall 2024
    ## 2119 18-Dec                        Pancake Single     2 Breakfast   Fall 2024
    ## 2120 18-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 2121 18-Dec                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 2122 18-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 2123 18-Dec           Create Your Pasta Bowl MEAT    25   Italian   Fall 2024
    ## 2124 18-Dec                   Pizza with Toppings    19   Italian   Fall 2024
    ## 2125 18-Dec            Create Your Pasta Bowl VEG    11   Italian   Fall 2024
    ## 2126 18-Dec                          Pizza Cheese    10   Italian   Fall 2024
    ## 2127 18-Dec                        Add Extra Meat     4   Italian   Fall 2024
    ## 2128 18-Dec                      Burrito Bowl BYO    27   Mexican   Fall 2024
    ## 2129 18-Dec                           Single Taco     3   Mexican   Fall 2024
    ## 2130 18-Dec                            Soup 12 oz    20      Soup   Fall 2024
    ## 2131 18-Dec                             8 oz Soup    16      Soup   Fall 2024
    ## 2132 18-Dec                    Salad by the Pound    14 Salad Bar   Fall 2024
    ## 2133 18-Dec                      Side Potato Tots     7 Grab N Go   Fall 2024
    ## 2134 19-Dec            Quesadilla Deluxe Trillium    59     Grill   Fall 2024
    ## 2135 19-Dec                     Grilled Hamburger    33     Grill   Fall 2024
    ## 2136 19-Dec         Burrito Una Mano Trillium BYO    17     Grill   Fall 2024
    ## 2137 19-Dec                          French Fries    44     Grill   Fall 2024
    ## 2138 19-Dec                 Fried Chicken Tenders    16 Grab N Go   Fall 2024
    ## 2139 19-Dec      Trillium Grill Impossible Burger     7     Grill   Fall 2024
    ## 2140 19-Dec       Grilled Chicken Breast Sandwich     6     Grill   Fall 2024
    ## 2141 19-Dec                     Quesadilla Cheese     6     Grill   Fall 2024
    ## 2142 19-Dec                    Sweet Potato Fries    15     Grill   Fall 2024
    ## 2143 19-Dec                  Seared Salmon Burger     4     Grill   Fall 2024
    ## 2144 19-Dec                    ADD Chicken Breast     1     Grill   Fall 2024
    ## 2145 19-Dec                          + Beef Patty     1     Grill   Fall 2024
    ## 2146 19-Dec                           Add Egg .99     3     Grill   Fall 2024
    ## 2147 19-Dec                            ADD Cheese     2     Grill   Fall 2024
    ## 2148 19-Dec                     1 Entree + 1 Side    40     Asian   Fall 2024
    ## 2149 19-Dec                     1 Entree + 2 Side    23     Asian   Fall 2024
    ## 2150 19-Dec                   2 Entrees + 2 Sides     7     Asian   Fall 2024
    ## 2151 19-Dec                    Bowl Ramen Chicken    10     Asian   Fall 2024
    ## 2152 19-Dec                       Bowl Ramen Tofu     4     Asian   Fall 2024
    ## 2153 19-Dec                          1 Wok Entree     1     Asian   Fall 2024
    ## 2154 19-Dec               Side Vegetarian Lo Mein     1     Asian   Fall 2024
    ## 2155 19-Dec              Side White or Brown Rice     1     Asian   Fall 2024
    ## 2156 19-Dec                     Burrito Breakfast    21 Breakfast   Fall 2024
    ## 2157 19-Dec                   Small French Omelet    13 Breakfast   Fall 2024
    ## 2158 19-Dec                  Grand Slam Breakfast     6 Breakfast   Fall 2024
    ## 2159 19-Dec                             Add Bacon     8 Breakfast   Fall 2024
    ## 2160 19-Dec      Egg Cheese Bacon Breakfast Sandw     2 Breakfast   Fall 2024
    ## 2161 19-Dec                              Two Eggs     3 Breakfast   Fall 2024
    ## 2162 19-Dec                   Trillium Home Fries     1 Breakfast   Fall 2024
    ## 2163 19-Dec                        2 Slices Toast     1 Breakfast   Fall 2024
    ## 2164 19-Dec                                 Toast     1 Breakfast   Fall 2024
    ## 2165 19-Dec           Create Your Pasta Bowl MEAT    12   Italian   Fall 2024
    ## 2166 19-Dec                   Pizza with Toppings    13   Italian   Fall 2024
    ## 2167 19-Dec                          Pizza Cheese    11   Italian   Fall 2024
    ## 2168 19-Dec            Create Your Pasta Bowl VEG     4   Italian   Fall 2024
    ## 2169 19-Dec                        Add Extra Meat     4   Italian   Fall 2024
    ## 2170 19-Dec                      Burrito Bowl BYO    26   Mexican   Fall 2024
    ## 2171 19-Dec                            Soup 12 oz    14      Soup   Fall 2024
    ## 2172 19-Dec                             8 oz Soup     8      Soup   Fall 2024
    ## 2173 19-Dec                    Salad by the Pound    10 Salad Bar   Fall 2024
    ## 2174 19-Dec                      Side Potato Tots     3 Grab N Go   Fall 2024
    ## 2175 20-Dec            Quesadilla Deluxe Trillium    40     Grill   Fall 2024
    ## 2176 20-Dec                     Grilled Hamburger    24     Grill   Fall 2024
    ## 2177 20-Dec         Burrito Una Mano Trillium BYO    15     Grill   Fall 2024
    ## 2178 20-Dec                          French Fries    29     Grill   Fall 2024
    ## 2179 20-Dec                  Seared Salmon Burger     8     Grill   Fall 2024
    ## 2180 20-Dec                 Fried Chicken Tenders     9 Grab N Go   Fall 2024
    ## 2181 20-Dec                     Quesadilla Cheese     6     Grill   Fall 2024
    ## 2182 20-Dec                    Sweet Potato Fries    11     Grill   Fall 2024
    ## 2183 20-Dec      Trillium Grill Impossible Burger     3     Grill   Fall 2024
    ## 2184 20-Dec       Grilled Chicken Breast Sandwich     3     Grill   Fall 2024
    ## 2185 20-Dec                     Black Bean Burger     1     Grill   Fall 2024
    ## 2186 20-Dec                   Add Sausage 2 Patty     3     Grill   Fall 2024
    ## 2187 20-Dec                          + Beef Patty     2     Grill   Fall 2024
    ## 2188 20-Dec                           Add Egg .99     1     Grill   Fall 2024
    ## 2189 20-Dec                    Bowl Ramen Chicken    20     Asian   Fall 2024
    ## 2190 20-Dec                     1 Entree + 1 Side    20     Asian   Fall 2024
    ## 2191 20-Dec                     1 Entree + 2 Side    15     Asian   Fall 2024
    ## 2192 20-Dec                   2 Entrees + 2 Sides    10     Asian   Fall 2024
    ## 2193 20-Dec                       Bowl Ramen Tofu     2     Asian   Fall 2024
    ## 2194 20-Dec                    Bowl Ramen Chicken     1     Asian   Fall 2024
    ## 2195 20-Dec                          1 Wok Entree     1     Asian   Fall 2024
    ## 2196 20-Dec                     Burrito Breakfast    28 Breakfast   Fall 2024
    ## 2197 20-Dec                  Grand Slam Breakfast     8 Breakfast   Fall 2024
    ## 2198 20-Dec                   Small French Omelet     8 Breakfast   Fall 2024
    ## 2199 20-Dec                             Add Bacon     7 Breakfast   Fall 2024
    ## 2200 20-Dec                              Two Eggs     4 Breakfast   Fall 2024
    ## 2201 20-Dec                        2 Slices Toast     3 Breakfast   Fall 2024
    ## 2202 20-Dec           Create Your Pasta Bowl MEAT    16   Italian   Fall 2024
    ## 2203 20-Dec            Create Your Pasta Bowl VEG     5   Italian   Fall 2024
    ## 2204 20-Dec                   Pizza with Toppings     6   Italian   Fall 2024
    ## 2205 20-Dec                          Pizza Cheese     5   Italian   Fall 2024
    ## 2206 20-Dec                        Add Extra Meat     1   Italian   Fall 2024
    ## 2207 20-Dec                      Burrito Bowl BYO    14   Mexican   Fall 2024
    ## 2208 20-Dec                           Single Taco     3   Mexican   Fall 2024
    ## 2209 20-Dec                        Side Guacamole     1   Mexican   Fall 2024
    ## 2210 20-Dec           Add Extra Toppings Una Mano     1   Mexican   Fall 2024
    ## 2211 20-Dec                            Soup 12 oz    14      Soup   Fall 2024
    ## 2212 20-Dec                             8 oz Soup    15      Soup   Fall 2024
    ## 2213 20-Dec                    Salad by the Pound    10 Salad Bar   Fall 2024
    ## 2214 20-Dec                      Side Potato Tots     7 Grab N Go   Fall 2024
    ## 2215 21-Jan            Quesadilla Deluxe Trillium   160     Grill Spring 2025
    ## 2216 21-Jan                     Grilled Hamburger    94     Grill Spring 2025
    ## 2217 21-Jan                 Fried Chicken Tenders   101 Grab N Go Spring 2025
    ## 2218 21-Jan         Burrito Una Mano Trillium BYO    66     Grill Spring 2025
    ## 2219 21-Jan                          French Fries   142     Grill Spring 2025
    ## 2220 21-Jan       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 2221 21-Jan                     Quesadilla Cheese    13     Grill Spring 2025
    ## 2222 21-Jan                    Sweet Potato Fries    30     Grill Spring 2025
    ## 2223 21-Jan      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 2224 21-Jan                          + Beef Patty    15     Grill Spring 2025
    ## 2225 21-Jan                  Seared Salmon Burger     6     Grill Spring 2025
    ## 2226 21-Jan                      Side Potato Tots    15 Grab N Go Spring 2025
    ## 2227 21-Jan                    ADD Chicken Breast     6     Grill Spring 2025
    ## 2228 21-Jan                     Black Bean Burger     2     Grill Spring 2025
    ## 2229 21-Jan                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 2230 21-Jan                            ADD Cheese    15     Grill Spring 2025
    ## 2231 21-Jan                           Add Egg .99     4     Grill Spring 2025
    ## 2232 21-Jan                     1 Entree + 1 Side   219     Asian Spring 2025
    ## 2233 21-Jan                    Bowl Ramen Chicken    83     Asian Spring 2025
    ## 2234 21-Jan                     1 Entree + 2 Side    75     Asian Spring 2025
    ## 2235 21-Jan                   2 Entrees + 2 Sides    30     Asian Spring 2025
    ## 2236 21-Jan                       Bowl Ramen Tofu    19     Asian Spring 2025
    ## 2237 21-Jan                          1 Wok Entree     7     Asian Spring 2025
    ## 2238 21-Jan               Side Vegetarian Lo Mein    10     Asian Spring 2025
    ## 2239 21-Jan       Side Vegetarian Fried Rice with     4     Asian Spring 2025
    ## 2240 21-Jan              Side White or Brown Rice     7     Asian Spring 2025
    ## 2241 21-Jan           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 2242 21-Jan                Side Fried Spring Roll     1     Asian Spring 2025
    ## 2243 21-Jan           Create Your Pasta Bowl MEAT   138   Italian Spring 2025
    ## 2244 21-Jan            Create Your Pasta Bowl VEG    42   Italian Spring 2025
    ## 2245 21-Jan                   Pizza with Toppings    34   Italian Spring 2025
    ## 2246 21-Jan                          Pizza Cheese    24   Italian Spring 2025
    ## 2247 21-Jan                        Add Extra Meat    14   Italian Spring 2025
    ## 2248 21-Jan                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 2249 21-Jan                   Small French Omelet    46 Breakfast Spring 2025
    ## 2250 21-Jan      Egg Cheese Bacon Breakfast Sandw    22 Breakfast Spring 2025
    ## 2251 21-Jan                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 2252 21-Jan      Egg Cheese Sausage Breakfast San    21 Breakfast Spring 2025
    ## 2253 21-Jan                             Add Bacon    23 Breakfast Spring 2025
    ## 2254 21-Jan                              Two Eggs    19 Breakfast Spring 2025
    ## 2255 21-Jan                        Pancake Single     3 Breakfast Spring 2025
    ## 2256 21-Jan                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 2257 21-Jan                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2258 21-Jan                                 Toast     1 Breakfast Spring 2025
    ## 2259 21-Jan                              PC Jelly     1 Breakfast Spring 2025
    ## 2260 21-Jan                      Burrito Bowl BYO   148   Mexican Spring 2025
    ## 2261 21-Jan                           Single Taco     4   Mexican Spring 2025
    ## 2262 21-Jan                    Salad by the Pound    58 Salad Bar Spring 2025
    ## 2263 21-Jan                            Soup 12 oz    43      Soup Spring 2025
    ## 2264 21-Jan                             8 oz Soup    41      Soup Spring 2025
    ## 2265 22-Jan            Quesadilla Deluxe Trillium   157     Grill Spring 2025
    ## 2266 22-Jan                     Grilled Hamburger   105     Grill Spring 2025
    ## 2267 22-Jan                 Fried Chicken Tenders    87 Grab N Go Spring 2025
    ## 2268 22-Jan         Burrito Una Mano Trillium BYO    64     Grill Spring 2025
    ## 2269 22-Jan                          French Fries   104     Grill Spring 2025
    ## 2270 22-Jan       Grilled Chicken Breast Sandwich    14     Grill Spring 2025
    ## 2271 22-Jan      Trillium Grill Impossible Burger    11     Grill Spring 2025
    ## 2272 22-Jan                     Quesadilla Cheese    13     Grill Spring 2025
    ## 2273 22-Jan                    Sweet Potato Fries    33     Grill Spring 2025
    ## 2274 22-Jan                          + Beef Patty    21     Grill Spring 2025
    ## 2275 22-Jan                  Seared Salmon Burger     9     Grill Spring 2025
    ## 2276 22-Jan                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 2277 22-Jan                     Black Bean Burger     3     Grill Spring 2025
    ## 2278 22-Jan                    ADD Chicken Breast     6     Grill Spring 2025
    ## 2279 22-Jan                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 2280 22-Jan                            ADD Cheese     9     Grill Spring 2025
    ## 2281 22-Jan                           Add Egg .99     3     Grill Spring 2025
    ## 2282 22-Jan                     1 Entree + 1 Side   229     Asian Spring 2025
    ## 2283 22-Jan                    Bowl Ramen Chicken    90     Asian Spring 2025
    ## 2284 22-Jan                     1 Entree + 2 Side    71     Asian Spring 2025
    ## 2285 22-Jan                   2 Entrees + 2 Sides    28     Asian Spring 2025
    ## 2286 22-Jan                       Bowl Ramen Tofu    12     Asian Spring 2025
    ## 2287 22-Jan                          1 Wok Entree     9     Asian Spring 2025
    ## 2288 22-Jan               Side Vegetarian Lo Mein    12     Asian Spring 2025
    ## 2289 22-Jan                       Side Vegetables     5     Asian Spring 2025
    ## 2290 22-Jan              Side White or Brown Rice     7     Asian Spring 2025
    ## 2291 22-Jan       Side Vegetarian Fried Rice with     3     Asian Spring 2025
    ## 2292 22-Jan           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 2293 22-Jan           Create Your Pasta Bowl MEAT   175   Italian Spring 2025
    ## 2294 22-Jan            Create Your Pasta Bowl VEG    23   Italian Spring 2025
    ## 2295 22-Jan                   Pizza with Toppings    32   Italian Spring 2025
    ## 2296 22-Jan                        Add Extra Meat    38   Italian Spring 2025
    ## 2297 22-Jan                          Pizza Cheese    22   Italian Spring 2025
    ## 2298 22-Jan                     Burrito Breakfast    93 Breakfast Spring 2025
    ## 2299 22-Jan                   Small French Omelet    53 Breakfast Spring 2025
    ## 2300 22-Jan                  Grand Slam Breakfast    17 Breakfast Spring 2025
    ## 2301 22-Jan                             Add Bacon    37 Breakfast Spring 2025
    ## 2302 22-Jan                              Two Eggs    16 Breakfast Spring 2025
    ## 2303 22-Jan                   Trillium Home Fries     9 Breakfast Spring 2025
    ## 2304 22-Jan                        Pancake Single     1 Breakfast Spring 2025
    ## 2305 22-Jan                                 Toast     2 Breakfast Spring 2025
    ## 2306 22-Jan                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2307 22-Jan                      Burrito Bowl BYO   122   Mexican Spring 2025
    ## 2308 22-Jan                        Side Guacamole     2   Mexican Spring 2025
    ## 2309 22-Jan                    Salad by the Pound    68 Salad Bar Spring 2025
    ## 2310 22-Jan                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 2311 22-Jan                            Soup 12 oz    55      Soup Spring 2025
    ## 2312 22-Jan                             8 oz Soup    42      Soup Spring 2025
    ## 2313 22-Jan   Egg Cheese Bacon Breakfast Sandwich    27 Grab N Go Spring 2025
    ## 2314 22-Jan Egg Cheese Sausage Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 2315 23-Jan            Quesadilla Deluxe Trillium   182     Grill Spring 2025
    ## 2316 23-Jan                     Grilled Hamburger    94     Grill Spring 2025
    ## 2317 23-Jan         Burrito Una Mano Trillium BYO    72     Grill Spring 2025
    ## 2318 23-Jan                 Fried Chicken Tenders    91 Grab N Go Spring 2025
    ## 2319 23-Jan                          French Fries   143     Grill Spring 2025
    ## 2320 23-Jan       Grilled Chicken Breast Sandwich    15     Grill Spring 2025
    ## 2321 23-Jan                     Quesadilla Cheese    12     Grill Spring 2025
    ## 2322 23-Jan      Trillium Grill Impossible Burger     9     Grill Spring 2025
    ## 2323 23-Jan                  Seared Salmon Burger    11     Grill Spring 2025
    ## 2324 23-Jan                    Sweet Potato Fries    27     Grill Spring 2025
    ## 2325 23-Jan                          + Beef Patty    19     Grill Spring 2025
    ## 2326 23-Jan                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 2327 23-Jan                    ADD Chicken Breast     6     Grill Spring 2025
    ## 2328 23-Jan                     Black Bean Burger     2     Grill Spring 2025
    ## 2329 23-Jan                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 2330 23-Jan                            ADD Cheese     8     Grill Spring 2025
    ## 2331 23-Jan                           Add Egg .99     3     Grill Spring 2025
    ## 2332 23-Jan                     1 Entree + 1 Side   230     Asian Spring 2025
    ## 2333 23-Jan                    Bowl Ramen Chicken    92     Asian Spring 2025
    ## 2334 23-Jan                     1 Entree + 2 Side    75     Asian Spring 2025
    ## 2335 23-Jan                   2 Entrees + 2 Sides    32     Asian Spring 2025
    ## 2336 23-Jan                       Bowl Ramen Tofu    18     Asian Spring 2025
    ## 2337 23-Jan               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 2338 23-Jan                          1 Wok Entree     3     Asian Spring 2025
    ## 2339 23-Jan              Side White or Brown Rice     5     Asian Spring 2025
    ## 2340 23-Jan           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 2341 23-Jan       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2342 23-Jan           Create Your Pasta Bowl MEAT   105   Italian Spring 2025
    ## 2343 23-Jan            Create Your Pasta Bowl VEG    27   Italian Spring 2025
    ## 2344 23-Jan                   Pizza with Toppings    28   Italian Spring 2025
    ## 2345 23-Jan                          Pizza Cheese    18   Italian Spring 2025
    ## 2346 23-Jan                        Add Extra Meat    21   Italian Spring 2025
    ## 2347 23-Jan              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2348 23-Jan                     Burrito Breakfast    86 Breakfast Spring 2025
    ## 2349 23-Jan                   Small French Omelet    41 Breakfast Spring 2025
    ## 2350 23-Jan                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 2351 23-Jan                             Add Bacon    37 Breakfast Spring 2025
    ## 2352 23-Jan                              Two Eggs    17 Breakfast Spring 2025
    ## 2353 23-Jan                        Pancake Single     4 Breakfast Spring 2025
    ## 2354 23-Jan                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2355 23-Jan                                 Toast     1 Breakfast Spring 2025
    ## 2356 23-Jan                              PC Jelly     1 Breakfast Spring 2025
    ## 2357 23-Jan                      Burrito Bowl BYO   122   Mexican Spring 2025
    ## 2358 23-Jan                           Single Taco     9   Mexican Spring 2025
    ## 2359 23-Jan                        Side Guacamole     3   Mexican Spring 2025
    ## 2360 23-Jan           Add Extra Toppings Una Mano     3   Mexican Spring 2025
    ## 2361 23-Jan                            Side Salsa     1   Mexican Spring 2025
    ## 2362 23-Jan                    Salad by the Pound    51 Salad Bar Spring 2025
    ## 2363 23-Jan                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 2364 23-Jan                            Soup 12 oz    36      Soup Spring 2025
    ## 2365 23-Jan                             8 oz Soup    31      Soup Spring 2025
    ## 2366 23-Jan   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 2367 23-Jan Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2368 24-Jan            Quesadilla Deluxe Trillium   129     Grill Spring 2025
    ## 2369 24-Jan                 Fried Chicken Tenders    79 Grab N Go Spring 2025
    ## 2370 24-Jan                     Grilled Hamburger    63     Grill Spring 2025
    ## 2371 24-Jan         Burrito Una Mano Trillium BYO    53     Grill Spring 2025
    ## 2372 24-Jan                          French Fries   102     Grill Spring 2025
    ## 2373 24-Jan       Grilled Chicken Breast Sandwich    13     Grill Spring 2025
    ## 2374 24-Jan      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 2375 24-Jan                     Quesadilla Cheese     9     Grill Spring 2025
    ## 2376 24-Jan                    Sweet Potato Fries    23     Grill Spring 2025
    ## 2377 24-Jan                          + Beef Patty    15     Grill Spring 2025
    ## 2378 24-Jan                  Seared Salmon Burger     5     Grill Spring 2025
    ## 2379 24-Jan                      Side Potato Tots     7 Grab N Go Spring 2025
    ## 2380 24-Jan                    ADD Chicken Breast     5     Grill Spring 2025
    ## 2381 24-Jan                     Black Bean Burger     1     Grill Spring 2025
    ## 2382 24-Jan                            ADD Cheese     7     Grill Spring 2025
    ## 2383 24-Jan                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 2384 24-Jan                           Add Egg .99     2     Grill Spring 2025
    ## 2385 24-Jan                     1 Entree + 1 Side   122     Asian Spring 2025
    ## 2386 24-Jan                     1 Entree + 2 Side    65     Asian Spring 2025
    ## 2387 24-Jan                    Bowl Ramen Chicken    67     Asian Spring 2025
    ## 2388 24-Jan                   2 Entrees + 2 Sides    14     Asian Spring 2025
    ## 2389 24-Jan                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 2390 24-Jan               Side Vegetarian Lo Mein     9     Asian Spring 2025
    ## 2391 24-Jan                          1 Wok Entree     4     Asian Spring 2025
    ## 2392 24-Jan              Side White or Brown Rice     7     Asian Spring 2025
    ## 2393 24-Jan                       Side Vegetables     3     Asian Spring 2025
    ## 2394 24-Jan                Side Fried Spring Roll     1     Asian Spring 2025
    ## 2395 24-Jan           Create Your Pasta Bowl MEAT    86   Italian Spring 2025
    ## 2396 24-Jan                   Pizza with Toppings    26   Italian Spring 2025
    ## 2397 24-Jan            Create Your Pasta Bowl VEG    15   Italian Spring 2025
    ## 2398 24-Jan                          Pizza Cheese    14   Italian Spring 2025
    ## 2399 24-Jan                        Add Extra Meat    19   Italian Spring 2025
    ## 2400 24-Jan                     Burrito Breakfast    70 Breakfast Spring 2025
    ## 2401 24-Jan                   Small French Omelet    50 Breakfast Spring 2025
    ## 2402 24-Jan                  Grand Slam Breakfast    10 Breakfast Spring 2025
    ## 2403 24-Jan                             Add Bacon    19 Breakfast Spring 2025
    ## 2404 24-Jan                        Pancake Single     6 Breakfast Spring 2025
    ## 2405 24-Jan                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 2406 24-Jan                              Two Eggs     3 Breakfast Spring 2025
    ## 2407 24-Jan                      Burrito Bowl BYO    56   Mexican Spring 2025
    ## 2408 24-Jan                        Side Guacamole     2   Mexican Spring 2025
    ## 2409 24-Jan                            Side Salsa     1   Mexican Spring 2025
    ## 2410 24-Jan           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2411 24-Jan                            Soup 12 oz    38      Soup Spring 2025
    ## 2412 24-Jan                             8 oz Soup    30      Soup Spring 2025
    ## 2413 24-Jan                    Salad by the Pound    38 Salad Bar Spring 2025
    ## 2414 24-Jan                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 2415 24-Jan Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 2416 24-Jan   Egg Cheese Bacon Breakfast Sandwich    17 Grab N Go Spring 2025
    ## 2417 27-Jan            Quesadilla Deluxe Trillium   176     Grill Spring 2025
    ## 2418 27-Jan                     Grilled Hamburger    92     Grill Spring 2025
    ## 2419 27-Jan                 Fried Chicken Tenders    90 Grab N Go Spring 2025
    ## 2420 27-Jan         Burrito Una Mano Trillium BYO    71     Grill Spring 2025
    ## 2421 27-Jan                          French Fries   113     Grill Spring 2025
    ## 2422 27-Jan       Grilled Chicken Breast Sandwich    22     Grill Spring 2025
    ## 2423 27-Jan      Trillium Grill Impossible Burger    12     Grill Spring 2025
    ## 2424 27-Jan                    Sweet Potato Fries    40     Grill Spring 2025
    ## 2425 27-Jan                  Seared Salmon Burger    10     Grill Spring 2025
    ## 2426 27-Jan                          + Beef Patty    16     Grill Spring 2025
    ## 2427 27-Jan                     Quesadilla Cheese     7     Grill Spring 2025
    ## 2428 27-Jan                      Side Potato Tots    12 Grab N Go Spring 2025
    ## 2429 27-Jan                     Black Bean Burger     4     Grill Spring 2025
    ## 2430 27-Jan                    ADD Chicken Breast     7     Grill Spring 2025
    ## 2431 27-Jan                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 2432 27-Jan                            ADD Cheese     6     Grill Spring 2025
    ## 2433 27-Jan                           Add Egg .99     2     Grill Spring 2025
    ## 2434 27-Jan                     1 Entree + 1 Side   197     Asian Spring 2025
    ## 2435 27-Jan                    Bowl Ramen Chicken    79     Asian Spring 2025
    ## 2436 27-Jan                     1 Entree + 2 Side    67     Asian Spring 2025
    ## 2437 27-Jan                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 2438 27-Jan                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 2439 27-Jan               Side Vegetarian Lo Mein    11     Asian Spring 2025
    ## 2440 27-Jan                          1 Wok Entree     4     Asian Spring 2025
    ## 2441 27-Jan                       Side Vegetables     3     Asian Spring 2025
    ## 2442 27-Jan       Side Vegetarian Fried Rice with     3     Asian Spring 2025
    ## 2443 27-Jan              Side White or Brown Rice     5     Asian Spring 2025
    ## 2444 27-Jan                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 2445 27-Jan           Create Your Pasta Bowl MEAT   162   Italian Spring 2025
    ## 2446 27-Jan            Create Your Pasta Bowl VEG    21   Italian Spring 2025
    ## 2447 27-Jan                   Pizza with Toppings    26   Italian Spring 2025
    ## 2448 27-Jan                        Add Extra Meat    34   Italian Spring 2025
    ## 2449 27-Jan                          Pizza Cheese    16   Italian Spring 2025
    ## 2450 27-Jan              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2451 27-Jan                     Burrito Breakfast    93 Breakfast Spring 2025
    ## 2452 27-Jan                   Small French Omelet    52 Breakfast Spring 2025
    ## 2453 27-Jan                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 2454 27-Jan                             Add Bacon    24 Breakfast Spring 2025
    ## 2455 27-Jan                              Two Eggs    10 Breakfast Spring 2025
    ## 2456 27-Jan                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 2457 27-Jan                        Pancake Single     3 Breakfast Spring 2025
    ## 2458 27-Jan                      Burrito Bowl BYO   133   Mexican Spring 2025
    ## 2459 27-Jan                           Single Taco     6   Mexican Spring 2025
    ## 2460 27-Jan                        Side Guacamole     1   Mexican Spring 2025
    ## 2461 27-Jan                    Salad by the Pound    72 Salad Bar Spring 2025
    ## 2462 27-Jan                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 2463 27-Jan                             8 oz Soup    59      Soup Spring 2025
    ## 2464 27-Jan                            Soup 12 oz    33      Soup Spring 2025
    ## 2465 27-Jan   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 2466 27-Jan Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2467 28-Jan            Quesadilla Deluxe Trillium   207     Grill Spring 2025
    ## 2468 28-Jan                     Grilled Hamburger   105     Grill Spring 2025
    ## 2469 28-Jan                 Fried Chicken Tenders   101 Grab N Go Spring 2025
    ## 2470 28-Jan         Burrito Una Mano Trillium BYO    67     Grill Spring 2025
    ## 2471 28-Jan                          French Fries   153     Grill Spring 2025
    ## 2472 28-Jan       Grilled Chicken Breast Sandwich    19     Grill Spring 2025
    ## 2473 28-Jan      Trillium Grill Impossible Burger    10     Grill Spring 2025
    ## 2474 28-Jan                    Sweet Potato Fries    34     Grill Spring 2025
    ## 2475 28-Jan                          + Beef Patty    27     Grill Spring 2025
    ## 2476 28-Jan                     Quesadilla Cheese     9     Grill Spring 2025
    ## 2477 28-Jan                    ADD Chicken Breast    17     Grill Spring 2025
    ## 2478 28-Jan                  Seared Salmon Burger     7     Grill Spring 2025
    ## 2479 28-Jan                      Side Potato Tots    18 Grab N Go Spring 2025
    ## 2480 28-Jan                     Black Bean Burger     5     Grill Spring 2025
    ## 2481 28-Jan                            ADD Cheese    13     Grill Spring 2025
    ## 2482 28-Jan                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 2483 28-Jan                           Add Egg .99     4     Grill Spring 2025
    ## 2484 28-Jan                     1 Entree + 1 Side   225     Asian Spring 2025
    ## 2485 28-Jan                    Bowl Ramen Chicken    88     Asian Spring 2025
    ## 2486 28-Jan                     1 Entree + 2 Side    72     Asian Spring 2025
    ## 2487 28-Jan                   2 Entrees + 2 Sides    30     Asian Spring 2025
    ## 2488 28-Jan                       Bowl Ramen Tofu    21     Asian Spring 2025
    ## 2489 28-Jan               Side Vegetarian Lo Mein    11     Asian Spring 2025
    ## 2490 28-Jan              Side White or Brown Rice    14     Asian Spring 2025
    ## 2491 28-Jan                          1 Wok Entree     3     Asian Spring 2025
    ## 2492 28-Jan           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 2493 28-Jan                Side Fried Spring Roll     1     Asian Spring 2025
    ## 2494 28-Jan       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2495 28-Jan           Create Your Pasta Bowl MEAT   102   Italian Spring 2025
    ## 2496 28-Jan            Create Your Pasta Bowl VEG    35   Italian Spring 2025
    ## 2497 28-Jan                   Pizza with Toppings    31   Italian Spring 2025
    ## 2498 28-Jan                          Pizza Cheese    25   Italian Spring 2025
    ## 2499 28-Jan                        Add Extra Meat    18   Italian Spring 2025
    ## 2500 28-Jan              Side Bread Pasta Station     3   Italian Spring 2025
    ## 2501 28-Jan                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 2502 28-Jan                   Small French Omelet    59 Breakfast Spring 2025
    ## 2503 28-Jan                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 2504 28-Jan                             Add Bacon    44 Breakfast Spring 2025
    ## 2505 28-Jan                              Two Eggs    14 Breakfast Spring 2025
    ## 2506 28-Jan                        Pancake Single     6 Breakfast Spring 2025
    ## 2507 28-Jan                        2 Slices Toast     4 Breakfast Spring 2025
    ## 2508 28-Jan                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 2509 28-Jan                      Burrito Bowl BYO   116   Mexican Spring 2025
    ## 2510 28-Jan                           Single Taco     4   Mexican Spring 2025
    ## 2511 28-Jan                        Side Guacamole     2   Mexican Spring 2025
    ## 2512 28-Jan           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 2513 28-Jan                             8 oz Soup    59      Soup Spring 2025
    ## 2514 28-Jan                            Soup 12 oz    47      Soup Spring 2025
    ## 2515 28-Jan                    Salad by the Pound    50 Salad Bar Spring 2025
    ## 2516 28-Jan                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 2517 28-Jan   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 2518 28-Jan Egg Cheese Sausage Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 2519 29-Jan            Quesadilla Deluxe Trillium   190     Grill Spring 2025
    ## 2520 29-Jan                     Grilled Hamburger    88     Grill Spring 2025
    ## 2521 29-Jan                 Fried Chicken Tenders   103 Grab N Go Spring 2025
    ## 2522 29-Jan         Burrito Una Mano Trillium BYO    72     Grill Spring 2025
    ## 2523 29-Jan                          French Fries   126     Grill Spring 2025
    ## 2524 29-Jan                     Quesadilla Cheese    14     Grill Spring 2025
    ## 2525 29-Jan       Grilled Chicken Breast Sandwich    12     Grill Spring 2025
    ## 2526 29-Jan                  Seared Salmon Burger    11     Grill Spring 2025
    ## 2527 29-Jan                    Sweet Potato Fries    31     Grill Spring 2025
    ## 2528 29-Jan      Trillium Grill Impossible Burger     5     Grill Spring 2025
    ## 2529 29-Jan                      Side Potato Tots    14 Grab N Go Spring 2025
    ## 2530 29-Jan                    ADD Chicken Breast    10     Grill Spring 2025
    ## 2531 29-Jan                     Black Bean Burger     4     Grill Spring 2025
    ## 2532 29-Jan                          + Beef Patty     8     Grill Spring 2025
    ## 2533 29-Jan           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 2534 29-Jan                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 2535 29-Jan             ADD Burger Salmon Grilled     1     Grill Spring 2025
    ## 2536 29-Jan                           Add Egg .99     4     Grill Spring 2025
    ## 2537 29-Jan                            ADD Cheese     5     Grill Spring 2025
    ## 2538 29-Jan                     1 Entree + 1 Side   206     Asian Spring 2025
    ## 2539 29-Jan                     1 Entree + 2 Side    94     Asian Spring 2025
    ## 2540 29-Jan                    Bowl Ramen Chicken    85     Asian Spring 2025
    ## 2541 29-Jan                   2 Entrees + 2 Sides    31     Asian Spring 2025
    ## 2542 29-Jan                       Bowl Ramen Tofu    21     Asian Spring 2025
    ## 2543 29-Jan               Side Vegetarian Lo Mein     9     Asian Spring 2025
    ## 2544 29-Jan                          1 Wok Entree     4     Asian Spring 2025
    ## 2545 29-Jan              Side White or Brown Rice     7     Asian Spring 2025
    ## 2546 29-Jan           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 2547 29-Jan           Create Your Pasta Bowl MEAT   150   Italian Spring 2025
    ## 2548 29-Jan                   Pizza with Toppings    38   Italian Spring 2025
    ## 2549 29-Jan            Create Your Pasta Bowl VEG    22   Italian Spring 2025
    ## 2550 29-Jan                          Pizza Cheese    23   Italian Spring 2025
    ## 2551 29-Jan                        Add Extra Meat    32   Italian Spring 2025
    ## 2552 29-Jan                     Burrito Breakfast    86 Breakfast Spring 2025
    ## 2553 29-Jan                   Small French Omelet    74 Breakfast Spring 2025
    ## 2554 29-Jan                  Grand Slam Breakfast    16 Breakfast Spring 2025
    ## 2555 29-Jan                             Add Bacon    18 Breakfast Spring 2025
    ## 2556 29-Jan                              Two Eggs    12 Breakfast Spring 2025
    ## 2557 29-Jan                        Pancake Single     5 Breakfast Spring 2025
    ## 2558 29-Jan                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 2559 29-Jan                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2560 29-Jan                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 2561 29-Jan                      Burrito Bowl BYO   120   Mexican Spring 2025
    ## 2562 29-Jan                           Single Taco    18   Mexican Spring 2025
    ## 2563 29-Jan           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2564 29-Jan                             8 oz Soup    80      Soup Spring 2025
    ## 2565 29-Jan                            Soup 12 oz    34      Soup Spring 2025
    ## 2566 29-Jan                    Salad by the Pound    52 Salad Bar Spring 2025
    ## 2567 29-Jan   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 2568 29-Jan Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2569 30-Jan            Quesadilla Deluxe Trillium   187     Grill Spring 2025
    ## 2570 30-Jan                     Grilled Hamburger   109     Grill Spring 2025
    ## 2571 30-Jan         Burrito Una Mano Trillium BYO    83     Grill Spring 2025
    ## 2572 30-Jan                 Fried Chicken Tenders    99 Grab N Go Spring 2025
    ## 2573 30-Jan                          French Fries   144     Grill Spring 2025
    ## 2574 30-Jan                    Sweet Potato Fries    47     Grill Spring 2025
    ## 2575 30-Jan                     Quesadilla Cheese    16     Grill Spring 2025
    ## 2576 30-Jan       Grilled Chicken Breast Sandwich    14     Grill Spring 2025
    ## 2577 30-Jan      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 2578 30-Jan                  Seared Salmon Burger     8     Grill Spring 2025
    ## 2579 30-Jan                          + Beef Patty    17     Grill Spring 2025
    ## 2580 30-Jan                      Side Potato Tots    12 Grab N Go Spring 2025
    ## 2581 30-Jan                     Black Bean Burger     3     Grill Spring 2025
    ## 2582 30-Jan             ADD Burger Salmon Grilled     2     Grill Spring 2025
    ## 2583 30-Jan                    ADD Chicken Breast     2     Grill Spring 2025
    ## 2584 30-Jan                           Add Egg .99     6     Grill Spring 2025
    ## 2585 30-Jan                            ADD Cheese     9     Grill Spring 2025
    ## 2586 30-Jan                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 2587 30-Jan                     1 Entree + 1 Side   209     Asian Spring 2025
    ## 2588 30-Jan                     1 Entree + 2 Side    93     Asian Spring 2025
    ## 2589 30-Jan                    Bowl Ramen Chicken    83     Asian Spring 2025
    ## 2590 30-Jan                   2 Entrees + 2 Sides    24     Asian Spring 2025
    ## 2591 30-Jan                       Bowl Ramen Tofu    26     Asian Spring 2025
    ## 2592 30-Jan               Side Vegetarian Lo Mein     4     Asian Spring 2025
    ## 2593 30-Jan              Side White or Brown Rice     7     Asian Spring 2025
    ## 2594 30-Jan                          1 Wok Entree     2     Asian Spring 2025
    ## 2595 30-Jan                Side Fried Spring Roll     1     Asian Spring 2025
    ## 2596 30-Jan                       Side Vegetables     1     Asian Spring 2025
    ## 2597 30-Jan       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2598 30-Jan           Create Your Pasta Bowl MEAT   118   Italian Spring 2025
    ## 2599 30-Jan            Create Your Pasta Bowl VEG    33   Italian Spring 2025
    ## 2600 30-Jan                   Pizza with Toppings    45   Italian Spring 2025
    ## 2601 30-Jan                          Pizza Cheese    27   Italian Spring 2025
    ## 2602 30-Jan                        Add Extra Meat    31   Italian Spring 2025
    ## 2603 30-Jan                     Burrito Breakfast    94 Breakfast Spring 2025
    ## 2604 30-Jan                   Small French Omelet    59 Breakfast Spring 2025
    ## 2605 30-Jan                  Grand Slam Breakfast     9 Breakfast Spring 2025
    ## 2606 30-Jan                             Add Bacon    29 Breakfast Spring 2025
    ## 2607 30-Jan                              Two Eggs    11 Breakfast Spring 2025
    ## 2608 30-Jan                        Pancake Single     3 Breakfast Spring 2025
    ## 2609 30-Jan                        2 Slices Toast     2 Breakfast Spring 2025
    ## 2610 30-Jan                              PC Jelly     1 Breakfast Spring 2025
    ## 2611 30-Jan                      Burrito Bowl BYO   102   Mexican Spring 2025
    ## 2612 30-Jan                           Single Taco     6   Mexican Spring 2025
    ## 2613 30-Jan                        Side Guacamole     4   Mexican Spring 2025
    ## 2614 30-Jan           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2615 30-Jan                    Salad by the Pound    64 Salad Bar Spring 2025
    ## 2616 30-Jan                Add Extra Protein 3.99     4 Salad Bar Spring 2025
    ## 2617 30-Jan                             8 oz Soup    39      Soup Spring 2025
    ## 2618 30-Jan                            Soup 12 oz    26      Soup Spring 2025
    ## 2619 30-Jan Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 2620 30-Jan   Egg Cheese Bacon Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 2621 31-Jan            Quesadilla Deluxe Trillium   129     Grill Spring 2025
    ## 2622 31-Jan                     Grilled Hamburger    64     Grill Spring 2025
    ## 2623 31-Jan         Burrito Una Mano Trillium BYO    60     Grill Spring 2025
    ## 2624 31-Jan                 Fried Chicken Tenders    60 Grab N Go Spring 2025
    ## 2625 31-Jan                          French Fries   111     Grill Spring 2025
    ## 2626 31-Jan       Grilled Chicken Breast Sandwich     9     Grill Spring 2025
    ## 2627 31-Jan                          + Beef Patty    21     Grill Spring 2025
    ## 2628 31-Jan                     Quesadilla Cheese     9     Grill Spring 2025
    ## 2629 31-Jan      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 2630 31-Jan                  Seared Salmon Burger     5     Grill Spring 2025
    ## 2631 31-Jan                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 2632 31-Jan                    ADD Chicken Breast     4     Grill Spring 2025
    ## 2633 31-Jan                            ADD Cheese    17     Grill Spring 2025
    ## 2634 31-Jan                     Black Bean Burger     1     Grill Spring 2025
    ## 2635 31-Jan                           Add Egg .99     5     Grill Spring 2025
    ## 2636 31-Jan                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 2637 31-Jan                     1 Entree + 1 Side   136     Asian Spring 2025
    ## 2638 31-Jan                    Bowl Ramen Chicken    71     Asian Spring 2025
    ## 2639 31-Jan                     1 Entree + 2 Side    62     Asian Spring 2025
    ## 2640 31-Jan                   2 Entrees + 2 Sides    22     Asian Spring 2025
    ## 2641 31-Jan                       Bowl Ramen Tofu    13     Asian Spring 2025
    ## 2642 31-Jan              Side White or Brown Rice     9     Asian Spring 2025
    ## 2643 31-Jan               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 2644 31-Jan                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 2645 31-Jan           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 2646 31-Jan       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2647 31-Jan           Create Your Pasta Bowl MEAT    90   Italian Spring 2025
    ## 2648 31-Jan                   Pizza with Toppings    35   Italian Spring 2025
    ## 2649 31-Jan                          Pizza Cheese    18   Italian Spring 2025
    ## 2650 31-Jan            Create Your Pasta Bowl VEG     8   Italian Spring 2025
    ## 2651 31-Jan                        Add Extra Meat    22   Italian Spring 2025
    ## 2652 31-Jan              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2653 31-Jan                     Burrito Breakfast    74 Breakfast Spring 2025
    ## 2654 31-Jan                   Small French Omelet    46 Breakfast Spring 2025
    ## 2655 31-Jan                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 2656 31-Jan                             Add Bacon    19 Breakfast Spring 2025
    ## 2657 31-Jan                              Two Eggs     9 Breakfast Spring 2025
    ## 2658 31-Jan                        Pancake Single     4 Breakfast Spring 2025
    ## 2659 31-Jan                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 2660 31-Jan                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2661 31-Jan                      Burrito Bowl BYO    68   Mexican Spring 2025
    ## 2662 31-Jan                           Single Taco     3   Mexican Spring 2025
    ## 2663 31-Jan                        Side Guacamole     4   Mexican Spring 2025
    ## 2664 31-Jan           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 2665 31-Jan                             8 oz Soup    37      Soup Spring 2025
    ## 2666 31-Jan                            Soup 12 oz    28      Soup Spring 2025
    ## 2667 31-Jan                    Salad by the Pound    38 Salad Bar Spring 2025
    ## 2668 31-Jan                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 2669 31-Jan   Egg Cheese Bacon Breakfast Sandwich    25 Grab N Go Spring 2025
    ## 2670 31-Jan Egg Cheese Sausage Breakfast Sandwich    17 Grab N Go Spring 2025
    ## 2671  3-Feb            Quesadilla Deluxe Trillium   192     Grill Spring 2025
    ## 2672  3-Feb                     Grilled Hamburger    90     Grill Spring 2025
    ## 2673  3-Feb         Burrito Una Mano Trillium BYO    73     Grill Spring 2025
    ## 2674  3-Feb                 Fried Chicken Tenders    73 Grab N Go Spring 2025
    ## 2675  3-Feb                          French Fries   128     Grill Spring 2025
    ## 2676  3-Feb       Grilled Chicken Breast Sandwich    15     Grill Spring 2025
    ## 2677  3-Feb      Trillium Grill Impossible Burger    12     Grill Spring 2025
    ## 2678  3-Feb                     Quesadilla Cheese    15     Grill Spring 2025
    ## 2679  3-Feb                  Seared Salmon Burger     8     Grill Spring 2025
    ## 2680  3-Feb                    Sweet Potato Fries    20     Grill Spring 2025
    ## 2681  3-Feb                          + Beef Patty    13     Grill Spring 2025
    ## 2682  3-Feb                      Side Potato Tots    15 Grab N Go Spring 2025
    ## 2683  3-Feb                    ADD Chicken Breast     6     Grill Spring 2025
    ## 2684  3-Feb                   Add Sausage 2 Patty     5     Grill Spring 2025
    ## 2685  3-Feb                            ADD Cheese    12     Grill Spring 2025
    ## 2686  3-Feb                           Add Egg .99     3     Grill Spring 2025
    ## 2687  3-Feb                     1 Entree + 1 Side   181     Asian Spring 2025
    ## 2688  3-Feb                     1 Entree + 2 Side   103     Asian Spring 2025
    ## 2689  3-Feb                    Bowl Ramen Chicken    81     Asian Spring 2025
    ## 2690  3-Feb                   2 Entrees + 2 Sides    29     Asian Spring 2025
    ## 2691  3-Feb                       Bowl Ramen Tofu    12     Asian Spring 2025
    ## 2692  3-Feb                          1 Wok Entree     8     Asian Spring 2025
    ## 2693  3-Feb               Side Vegetarian Lo Mein     8     Asian Spring 2025
    ## 2694  3-Feb              Side White or Brown Rice    10     Asian Spring 2025
    ## 2695  3-Feb           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 2696  3-Feb                       Side Vegetables     1     Asian Spring 2025
    ## 2697  3-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2698  3-Feb           Create Your Pasta Bowl MEAT   140   Italian Spring 2025
    ## 2699  3-Feb                   Pizza with Toppings    39   Italian Spring 2025
    ## 2700  3-Feb            Create Your Pasta Bowl VEG    18   Italian Spring 2025
    ## 2701  3-Feb                        Add Extra Meat    36   Italian Spring 2025
    ## 2702  3-Feb                          Pizza Cheese    16   Italian Spring 2025
    ## 2703  3-Feb              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2704  3-Feb                     Burrito Breakfast    85 Breakfast Spring 2025
    ## 2705  3-Feb                   Small French Omelet    66 Breakfast Spring 2025
    ## 2706  3-Feb                  Grand Slam Breakfast     7 Breakfast Spring 2025
    ## 2707  3-Feb                             Add Bacon    30 Breakfast Spring 2025
    ## 2708  3-Feb                              Two Eggs    27 Breakfast Spring 2025
    ## 2709  3-Feb                   Trillium Home Fries     5 Breakfast Spring 2025
    ## 2710  3-Feb                        Pancake Single     1 Breakfast Spring 2025
    ## 2711  3-Feb                        2 Slices Toast     2 Breakfast Spring 2025
    ## 2712  3-Feb                      PC Peanut Butter     2 Breakfast Spring 2025
    ## 2713  3-Feb                                 Toast     1 Breakfast Spring 2025
    ## 2714  3-Feb                      Burrito Bowl BYO   110   Mexican Spring 2025
    ## 2715  3-Feb                           Single Taco     4   Mexican Spring 2025
    ## 2716  3-Feb                        Side Guacamole     1   Mexican Spring 2025
    ## 2717  3-Feb                    Salad by the Pound    64 Salad Bar Spring 2025
    ## 2718  3-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 2719  3-Feb                             8 oz Soup    56      Soup Spring 2025
    ## 2720  3-Feb                            Soup 12 oz    38      Soup Spring 2025
    ## 2721  3-Feb   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 2722  3-Feb Egg Cheese Sausage Breakfast Sandwich    19 Grab N Go Spring 2025
    ## 2723  4-Feb            Quesadilla Deluxe Trillium   196     Grill Spring 2025
    ## 2724  4-Feb                     Grilled Hamburger   108     Grill Spring 2025
    ## 2725  4-Feb                 Fried Chicken Tenders   107 Grab N Go Spring 2025
    ## 2726  4-Feb         Burrito Una Mano Trillium BYO    75     Grill Spring 2025
    ## 2727  4-Feb                          French Fries   130     Grill Spring 2025
    ## 2728  4-Feb       Grilled Chicken Breast Sandwich    19     Grill Spring 2025
    ## 2729  4-Feb                    Sweet Potato Fries    45     Grill Spring 2025
    ## 2730  4-Feb                          + Beef Patty    21     Grill Spring 2025
    ## 2731  4-Feb                     Quesadilla Cheese     8     Grill Spring 2025
    ## 2732  4-Feb                  Seared Salmon Burger     7     Grill Spring 2025
    ## 2733  4-Feb                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 2734  4-Feb                    ADD Chicken Breast     8     Grill Spring 2025
    ## 2735  4-Feb                     Black Bean Burger     3     Grill Spring 2025
    ## 2736  4-Feb      Trillium Grill Impossible Burger     2     Grill Spring 2025
    ## 2737  4-Feb                   Add Sausage 2 Patty     5     Grill Spring 2025
    ## 2738  4-Feb                            ADD Cheese    16     Grill Spring 2025
    ## 2739  4-Feb                           Add Egg .99     7     Grill Spring 2025
    ## 2740  4-Feb             ADD Burger Salmon Grilled     1     Grill Spring 2025
    ## 2741  4-Feb                     1 Entree + 1 Side   213     Asian Spring 2025
    ## 2742  4-Feb                     1 Entree + 2 Side    82     Asian Spring 2025
    ## 2743  4-Feb                    Bowl Ramen Chicken    86     Asian Spring 2025
    ## 2744  4-Feb                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 2745  4-Feb                       Bowl Ramen Tofu    25     Asian Spring 2025
    ## 2746  4-Feb                          1 Wok Entree     8     Asian Spring 2025
    ## 2747  4-Feb               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 2748  4-Feb              Side White or Brown Rice     9     Asian Spring 2025
    ## 2749  4-Feb                       Side Vegetables     3     Asian Spring 2025
    ## 2750  4-Feb       Side Vegetarian Fried Rice with     3     Asian Spring 2025
    ## 2751  4-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 2752  4-Feb           Create Your Pasta Bowl MEAT   101   Italian Spring 2025
    ## 2753  4-Feb                   Pizza with Toppings    52   Italian Spring 2025
    ## 2754  4-Feb            Create Your Pasta Bowl VEG    24   Italian Spring 2025
    ## 2755  4-Feb                          Pizza Cheese    20   Italian Spring 2025
    ## 2756  4-Feb                        Add Extra Meat    17   Italian Spring 2025
    ## 2757  4-Feb              Side Bread Pasta Station     2   Italian Spring 2025
    ## 2758  4-Feb                     Burrito Breakfast    96 Breakfast Spring 2025
    ## 2759  4-Feb                   Small French Omelet    52 Breakfast Spring 2025
    ## 2760  4-Feb                  Grand Slam Breakfast    14 Breakfast Spring 2025
    ## 2761  4-Feb                             Add Bacon    26 Breakfast Spring 2025
    ## 2762  4-Feb                              Two Eggs    15 Breakfast Spring 2025
    ## 2763  4-Feb                        Pancake Single     4 Breakfast Spring 2025
    ## 2764  4-Feb                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 2765  4-Feb                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2766  4-Feb                      Burrito Bowl BYO   100   Mexican Spring 2025
    ## 2767  4-Feb                           Single Taco     7   Mexican Spring 2025
    ## 2768  4-Feb                        Side Guacamole     3   Mexican Spring 2025
    ## 2769  4-Feb                             8 oz Soup    60      Soup Spring 2025
    ## 2770  4-Feb                            Soup 12 oz    41      Soup Spring 2025
    ## 2771  4-Feb                    Salad by the Pound    50 Salad Bar Spring 2025
    ## 2772  4-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 2773  4-Feb Egg Cheese Sausage Breakfast Sandwich    28 Grab N Go Spring 2025
    ## 2774  4-Feb   Egg Cheese Bacon Breakfast Sandwich    18 Grab N Go Spring 2025
    ## 2775  5-Feb            Quesadilla Deluxe Trillium   185     Grill Spring 2025
    ## 2776  5-Feb                     Grilled Hamburger    97     Grill Spring 2025
    ## 2777  5-Feb         Burrito Una Mano Trillium BYO    72     Grill Spring 2025
    ## 2778  5-Feb                 Fried Chicken Tenders    81 Grab N Go Spring 2025
    ## 2779  5-Feb                          French Fries   124     Grill Spring 2025
    ## 2780  5-Feb       Grilled Chicken Breast Sandwich    23     Grill Spring 2025
    ## 2781  5-Feb                          + Beef Patty    24     Grill Spring 2025
    ## 2782  5-Feb                    Sweet Potato Fries    27     Grill Spring 2025
    ## 2783  5-Feb      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 2784  5-Feb                     Quesadilla Cheese     8     Grill Spring 2025
    ## 2785  5-Feb                      Side Potato Tots    20 Grab N Go Spring 2025
    ## 2786  5-Feb                  Seared Salmon Burger     6     Grill Spring 2025
    ## 2787  5-Feb                     Black Bean Burger     3     Grill Spring 2025
    ## 2788  5-Feb                    ADD Chicken Breast     4     Grill Spring 2025
    ## 2789  5-Feb                            ADD Cheese    21     Grill Spring 2025
    ## 2790  5-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 2791  5-Feb                           Add Egg .99     4     Grill Spring 2025
    ## 2792  5-Feb                     1 Entree + 1 Side   203     Asian Spring 2025
    ## 2793  5-Feb                     1 Entree + 2 Side    98     Asian Spring 2025
    ## 2794  5-Feb                    Bowl Ramen Chicken    78     Asian Spring 2025
    ## 2795  5-Feb                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 2796  5-Feb                       Bowl Ramen Tofu    14     Asian Spring 2025
    ## 2797  5-Feb                          1 Wok Entree     8     Asian Spring 2025
    ## 2798  5-Feb               Side Vegetarian Lo Mein     4     Asian Spring 2025
    ## 2799  5-Feb                       Side Vegetables     3     Asian Spring 2025
    ## 2800  5-Feb           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 2801  5-Feb              Side White or Brown Rice     3     Asian Spring 2025
    ## 2802  5-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2803  5-Feb           Create Your Pasta Bowl MEAT   144   Italian Spring 2025
    ## 2804  5-Feb                   Pizza with Toppings    42   Italian Spring 2025
    ## 2805  5-Feb            Create Your Pasta Bowl VEG    26   Italian Spring 2025
    ## 2806  5-Feb                        Add Extra Meat    40   Italian Spring 2025
    ## 2807  5-Feb                          Pizza Cheese    15   Italian Spring 2025
    ## 2808  5-Feb              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2809  5-Feb                     Burrito Breakfast    96 Breakfast Spring 2025
    ## 2810  5-Feb                   Small French Omelet    51 Breakfast Spring 2025
    ## 2811  5-Feb                  Grand Slam Breakfast    17 Breakfast Spring 2025
    ## 2812  5-Feb                             Add Bacon    31 Breakfast Spring 2025
    ## 2813  5-Feb                              Two Eggs    13 Breakfast Spring 2025
    ## 2814  5-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 2815  5-Feb                        Pancake Single     3 Breakfast Spring 2025
    ## 2816  5-Feb                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 2817  5-Feb                      Burrito Bowl BYO   139   Mexican Spring 2025
    ## 2818  5-Feb                        Side Guacamole     1   Mexican Spring 2025
    ## 2819  5-Feb                            Side Salsa     1   Mexican Spring 2025
    ## 2820  5-Feb           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2821  5-Feb                             8 oz Soup    58      Soup Spring 2025
    ## 2822  5-Feb                            Soup 12 oz    49      Soup Spring 2025
    ## 2823  5-Feb                    Salad by the Pound    47 Salad Bar Spring 2025
    ## 2824  5-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 2825  5-Feb Egg Cheese Sausage Breakfast Sandwich    34 Grab N Go Spring 2025
    ## 2826  5-Feb   Egg Cheese Bacon Breakfast Sandwich    29 Grab N Go Spring 2025
    ## 2827  6-Feb                     1 Entree + 1 Side   260     Asian Spring 2025
    ## 2828  6-Feb                     1 Entree + 2 Side    95     Asian Spring 2025
    ## 2829  6-Feb                    Bowl Ramen Chicken    98     Asian Spring 2025
    ## 2830  6-Feb                   2 Entrees + 2 Sides    62     Asian Spring 2025
    ## 2831  6-Feb                       Bowl Ramen Tofu    12     Asian Spring 2025
    ## 2832  6-Feb                          1 Wok Entree     7     Asian Spring 2025
    ## 2833  6-Feb               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 2834  6-Feb              Side White or Brown Rice     5     Asian Spring 2025
    ## 2835  6-Feb                Side Fried Spring Roll     2     Asian Spring 2025
    ## 2836  6-Feb           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 2837  6-Feb                       Side Vegetables     1     Asian Spring 2025
    ## 2838  6-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2839  6-Feb                 Fried Chicken Tenders   164 Grab N Go Spring 2025
    ## 2840  6-Feb         Burrito Una Mano Trillium BYO   101     Grill Spring 2025
    ## 2841  6-Feb                     Quesadilla Cheese    87     Grill Spring 2025
    ## 2842  6-Feb                          French Fries   147     Grill Spring 2025
    ## 2843  6-Feb                    Sweet Potato Fries    28     Grill Spring 2025
    ## 2844  6-Feb                           Add Egg .99     8     Grill Spring 2025
    ## 2845  6-Feb                      Side Potato Tots     2 Grab N Go Spring 2025
    ## 2846  6-Feb           Create Your Pasta Bowl MEAT   116   Italian Spring 2025
    ## 2847  6-Feb            Create Your Pasta Bowl VEG    42   Italian Spring 2025
    ## 2848  6-Feb                   Pizza with Toppings    61   Italian Spring 2025
    ## 2849  6-Feb                          Pizza Cheese    22   Italian Spring 2025
    ## 2850  6-Feb                        Add Extra Meat    18   Italian Spring 2025
    ## 2851  6-Feb              Side Bread Pasta Station     1   Italian Spring 2025
    ## 2852  6-Feb                      Burrito Bowl BYO   130   Mexican Spring 2025
    ## 2853  6-Feb                           Single Taco    11   Mexican Spring 2025
    ## 2854  6-Feb           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2855  6-Feb                   Small French Omelet    97 Breakfast Spring 2025
    ## 2856  6-Feb                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 2857  6-Feb                        2 Slices Toast     2 Breakfast Spring 2025
    ## 2858  6-Feb                                 Toast     1 Breakfast Spring 2025
    ## 2859  6-Feb                             8 oz Soup    60      Soup Spring 2025
    ## 2860  6-Feb                            Soup 12 oz    49      Soup Spring 2025
    ## 2861  6-Feb                    Salad by the Pound    49 Salad Bar Spring 2025
    ## 2862  6-Feb                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 2863  6-Feb   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2864  6-Feb            LTO Spicy Chicken Sandwich     1 Grab N Go Spring 2025
    ## 2865  7-Feb            Quesadilla Deluxe Trillium   123     Grill Spring 2025
    ## 2866  7-Feb                     Grilled Hamburger    86     Grill Spring 2025
    ## 2867  7-Feb         Burrito Una Mano Trillium BYO    50     Grill Spring 2025
    ## 2868  7-Feb                 Fried Chicken Tenders    60 Grab N Go Spring 2025
    ## 2869  7-Feb                          French Fries    99     Grill Spring 2025
    ## 2870  7-Feb                     Quesadilla Cheese    13     Grill Spring 2025
    ## 2871  7-Feb                    Sweet Potato Fries    24     Grill Spring 2025
    ## 2872  7-Feb                          + Beef Patty    18     Grill Spring 2025
    ## 2873  7-Feb                  Seared Salmon Burger     8     Grill Spring 2025
    ## 2874  7-Feb      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 2875  7-Feb       Grilled Chicken Breast Sandwich     7     Grill Spring 2025
    ## 2876  7-Feb                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 2877  7-Feb                     Black Bean Burger     2     Grill Spring 2025
    ## 2878  7-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 2879  7-Feb                            ADD Cheese     6     Grill Spring 2025
    ## 2880  7-Feb                           Add Egg .99     2     Grill Spring 2025
    ## 2881  7-Feb                     1 Entree + 1 Side   129     Asian Spring 2025
    ## 2882  7-Feb                     1 Entree + 2 Side    53     Asian Spring 2025
    ## 2883  7-Feb                    Bowl Ramen Chicken    43     Asian Spring 2025
    ## 2884  7-Feb                   2 Entrees + 2 Sides    22     Asian Spring 2025
    ## 2885  7-Feb                       Bowl Ramen Tofu    18     Asian Spring 2025
    ## 2886  7-Feb               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 2887  7-Feb           Side Vegetable Spring Rolls     4     Asian Spring 2025
    ## 2888  7-Feb                          1 Wok Entree     2     Asian Spring 2025
    ## 2889  7-Feb              Side White or Brown Rice     3     Asian Spring 2025
    ## 2890  7-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2891  7-Feb                     Burrito Breakfast    60 Breakfast Spring 2025
    ## 2892  7-Feb                   Small French Omelet    47 Breakfast Spring 2025
    ## 2893  7-Feb                  Grand Slam Breakfast    22 Breakfast Spring 2025
    ## 2894  7-Feb                             Add Bacon    34 Breakfast Spring 2025
    ## 2895  7-Feb                              Two Eggs    15 Breakfast Spring 2025
    ## 2896  7-Feb                        Pancake Single     8 Breakfast Spring 2025
    ## 2897  7-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 2898  7-Feb                        2 Slices Toast     1 Breakfast Spring 2025
    ## 2899  7-Feb                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 2900  7-Feb           Create Your Pasta Bowl MEAT    80   Italian Spring 2025
    ## 2901  7-Feb            Create Your Pasta Bowl VEG    15   Italian Spring 2025
    ## 2902  7-Feb                   Pizza with Toppings    18   Italian Spring 2025
    ## 2903  7-Feb                        Add Extra Meat    28   Italian Spring 2025
    ## 2904  7-Feb                          Pizza Cheese    10   Italian Spring 2025
    ## 2905  7-Feb                      Burrito Bowl BYO    50   Mexican Spring 2025
    ## 2906  7-Feb            LTO Spicy Chicken Sandwich    26 Grab N Go Spring 2025
    ## 2907  7-Feb   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2908  7-Feb Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 2909  7-Feb                            Soup 12 oz    32      Soup Spring 2025
    ## 2910  7-Feb                             8 oz Soup    35      Soup Spring 2025
    ## 2911  7-Feb                    Salad by the Pound    33 Salad Bar Spring 2025
    ## 2912 10-Feb            Quesadilla Deluxe Trillium   187     Grill Spring 2025
    ## 2913 10-Feb                     Grilled Hamburger   101     Grill Spring 2025
    ## 2914 10-Feb         Burrito Una Mano Trillium BYO    60     Grill Spring 2025
    ## 2915 10-Feb                 Fried Chicken Tenders    63 Grab N Go Spring 2025
    ## 2916 10-Feb                          French Fries    99     Grill Spring 2025
    ## 2917 10-Feb                    Sweet Potato Fries    40     Grill Spring 2025
    ## 2918 10-Feb       Grilled Chicken Breast Sandwich    14     Grill Spring 2025
    ## 2919 10-Feb                  Seared Salmon Burger    14     Grill Spring 2025
    ## 2920 10-Feb                     Quesadilla Cheese    13     Grill Spring 2025
    ## 2921 10-Feb                          + Beef Patty    17     Grill Spring 2025
    ## 2922 10-Feb                      Side Potato Tots    17 Grab N Go Spring 2025
    ## 2923 10-Feb      Trillium Grill Impossible Burger     4     Grill Spring 2025
    ## 2924 10-Feb                     Black Bean Burger     4     Grill Spring 2025
    ## 2925 10-Feb                    ADD Chicken Breast     5     Grill Spring 2025
    ## 2926 10-Feb                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 2927 10-Feb                           Add Egg .99     4     Grill Spring 2025
    ## 2928 10-Feb                            ADD Cheese     5     Grill Spring 2025
    ## 2929 10-Feb                     1 Entree + 1 Side   200     Asian Spring 2025
    ## 2930 10-Feb                     1 Entree + 2 Side    69     Asian Spring 2025
    ## 2931 10-Feb                    Bowl Ramen Chicken    58     Asian Spring 2025
    ## 2932 10-Feb                   2 Entrees + 2 Sides    18     Asian Spring 2025
    ## 2933 10-Feb                       Bowl Ramen Tofu    21     Asian Spring 2025
    ## 2934 10-Feb               Side Vegetarian Lo Mein    11     Asian Spring 2025
    ## 2935 10-Feb                          1 Wok Entree     6     Asian Spring 2025
    ## 2936 10-Feb              Side White or Brown Rice     6     Asian Spring 2025
    ## 2937 10-Feb                       Side Vegetables     2     Asian Spring 2025
    ## 2938 10-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 2939 10-Feb           Create Your Pasta Bowl MEAT   121   Italian Spring 2025
    ## 2940 10-Feb            Create Your Pasta Bowl VEG    19   Italian Spring 2025
    ## 2941 10-Feb                        Add Extra Meat    33   Italian Spring 2025
    ## 2942 10-Feb                          Pizza Cheese    16   Italian Spring 2025
    ## 2943 10-Feb                   Pizza with Toppings    12   Italian Spring 2025
    ## 2944 10-Feb                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 2945 10-Feb                   Small French Omelet    49 Breakfast Spring 2025
    ## 2946 10-Feb                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 2947 10-Feb                             Add Bacon    29 Breakfast Spring 2025
    ## 2948 10-Feb                              Two Eggs    11 Breakfast Spring 2025
    ## 2949 10-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 2950 10-Feb                        2 Slices Toast     4 Breakfast Spring 2025
    ## 2951 10-Feb                                 Toast     1 Breakfast Spring 2025
    ## 2952 10-Feb                      Burrito Bowl BYO   104   Mexican Spring 2025
    ## 2953 10-Feb                           Single Taco     8   Mexican Spring 2025
    ## 2954 10-Feb                        Side Guacamole     4   Mexican Spring 2025
    ## 2955 10-Feb           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 2956 10-Feb            LTO Spicy Chicken Sandwich    46 Grab N Go Spring 2025
    ## 2957 10-Feb   Egg Cheese Bacon Breakfast Sandwich    26 Grab N Go Spring 2025
    ## 2958 10-Feb Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 2959 10-Feb                    Salad by the Pound    52 Salad Bar Spring 2025
    ## 2960 10-Feb                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 2961 10-Feb                            Soup 12 oz    35      Soup Spring 2025
    ## 2962 10-Feb                             8 oz Soup    36      Soup Spring 2025
    ## 2963 11-Feb            Quesadilla Deluxe Trillium   199     Grill Spring 2025
    ## 2964 11-Feb                     Grilled Hamburger    86     Grill Spring 2025
    ## 2965 11-Feb                 Fried Chicken Tenders    95 Grab N Go Spring 2025
    ## 2966 11-Feb         Burrito Una Mano Trillium BYO    63     Grill Spring 2025
    ## 2967 11-Feb                          French Fries   142     Grill Spring 2025
    ## 2968 11-Feb       Grilled Chicken Breast Sandwich    26     Grill Spring 2025
    ## 2969 11-Feb                     Quesadilla Cheese    11     Grill Spring 2025
    ## 2970 11-Feb                    Sweet Potato Fries    28     Grill Spring 2025
    ## 2971 11-Feb                          + Beef Patty    18     Grill Spring 2025
    ## 2972 11-Feb                  Seared Salmon Burger     8     Grill Spring 2025
    ## 2973 11-Feb      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 2974 11-Feb                      Side Potato Tots    15 Grab N Go Spring 2025
    ## 2975 11-Feb                    ADD Chicken Breast     7     Grill Spring 2025
    ## 2976 11-Feb                     Black Bean Burger     3     Grill Spring 2025
    ## 2977 11-Feb                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 2978 11-Feb                            ADD Cheese     9     Grill Spring 2025
    ## 2979 11-Feb                           Add Egg .99     3     Grill Spring 2025
    ## 2980 11-Feb                     1 Entree + 1 Side   217     Asian Spring 2025
    ## 2981 11-Feb                     1 Entree + 2 Side    86     Asian Spring 2025
    ## 2982 11-Feb                    Bowl Ramen Chicken    94     Asian Spring 2025
    ## 2983 11-Feb                   2 Entrees + 2 Sides    33     Asian Spring 2025
    ## 2984 11-Feb                       Bowl Ramen Tofu    20     Asian Spring 2025
    ## 2985 11-Feb               Side Vegetarian Lo Mein    13     Asian Spring 2025
    ## 2986 11-Feb              Side White or Brown Rice    12     Asian Spring 2025
    ## 2987 11-Feb       Side Vegetarian Fried Rice with     5     Asian Spring 2025
    ## 2988 11-Feb                          1 Wok Entree     2     Asian Spring 2025
    ## 2989 11-Feb                       Side Vegetables     3     Asian Spring 2025
    ## 2990 11-Feb                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 2991 11-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 2992 11-Feb           Create Your Pasta Bowl MEAT   111   Italian Spring 2025
    ## 2993 11-Feb            Create Your Pasta Bowl VEG    23   Italian Spring 2025
    ## 2994 11-Feb                   Pizza with Toppings    23   Italian Spring 2025
    ## 2995 11-Feb                          Pizza Cheese    21   Italian Spring 2025
    ## 2996 11-Feb                        Add Extra Meat    20   Italian Spring 2025
    ## 2997 11-Feb                     Burrito Breakfast    79 Breakfast Spring 2025
    ## 2998 11-Feb                   Small French Omelet    52 Breakfast Spring 2025
    ## 2999 11-Feb                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3000 11-Feb                             Add Bacon    33 Breakfast Spring 2025
    ## 3001 11-Feb                              Two Eggs    15 Breakfast Spring 2025
    ## 3002 11-Feb                        Pancake Single     3 Breakfast Spring 2025
    ## 3003 11-Feb                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 3004 11-Feb                        2 Slices Toast     3 Breakfast Spring 2025
    ## 3005 11-Feb                                 Toast     2 Breakfast Spring 2025
    ## 3006 11-Feb                              PC Jelly     1 Breakfast Spring 2025
    ## 3007 11-Feb                      Burrito Bowl BYO   107   Mexican Spring 2025
    ## 3008 11-Feb                           Single Taco     4   Mexican Spring 2025
    ## 3009 11-Feb                        Side Guacamole     3   Mexican Spring 2025
    ## 3010 11-Feb            LTO Spicy Chicken Sandwich    35 Grab N Go Spring 2025
    ## 3011 11-Feb   Egg Cheese Bacon Breakfast Sandwich    25 Grab N Go Spring 2025
    ## 3012 11-Feb                      LTO Meatball Sub    14 Grab N Go Spring 2025
    ## 3013 11-Feb Egg Cheese Sausage Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 3014 11-Feb                    Salad by the Pound    51 Salad Bar Spring 2025
    ## 3015 11-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 3016 11-Feb                            Soup 12 oz    60      Soup Spring 2025
    ## 3017 11-Feb                             8 oz Soup    32      Soup Spring 2025
    ## 3018 12-Feb            Quesadilla Deluxe Trillium   184     Grill Spring 2025
    ## 3019 12-Feb                     Grilled Hamburger   101     Grill Spring 2025
    ## 3020 12-Feb                 Fried Chicken Tenders    86 Grab N Go Spring 2025
    ## 3021 12-Feb         Burrito Una Mano Trillium BYO    62     Grill Spring 2025
    ## 3022 12-Feb                          French Fries   117     Grill Spring 2025
    ## 3023 12-Feb       Grilled Chicken Breast Sandwich    21     Grill Spring 2025
    ## 3024 12-Feb                    Sweet Potato Fries    46     Grill Spring 2025
    ## 3025 12-Feb                     Quesadilla Cheese    14     Grill Spring 2025
    ## 3026 12-Feb                  Seared Salmon Burger    12     Grill Spring 2025
    ## 3027 12-Feb      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 3028 12-Feb                      Side Potato Tots    18 Grab N Go Spring 2025
    ## 3029 12-Feb                          + Beef Patty    13     Grill Spring 2025
    ## 3030 12-Feb                    ADD Chicken Breast     7     Grill Spring 2025
    ## 3031 12-Feb                     Black Bean Burger     3     Grill Spring 2025
    ## 3032 12-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3033 12-Feb                            ADD Cheese     6     Grill Spring 2025
    ## 3034 12-Feb                           Add Egg .99     2     Grill Spring 2025
    ## 3035 12-Feb                     1 Entree + 1 Side   218     Asian Spring 2025
    ## 3036 12-Feb                     1 Entree + 2 Side    81     Asian Spring 2025
    ## 3037 12-Feb                    Bowl Ramen Chicken    78     Asian Spring 2025
    ## 3038 12-Feb                   2 Entrees + 2 Sides    27     Asian Spring 2025
    ## 3039 12-Feb                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 3040 12-Feb                          1 Wok Entree     9     Asian Spring 2025
    ## 3041 12-Feb               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 3042 12-Feb       Side Vegetarian Fried Rice with     4     Asian Spring 2025
    ## 3043 12-Feb                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3044 12-Feb                       Side Vegetables     2     Asian Spring 2025
    ## 3045 12-Feb              Side White or Brown Rice     2     Asian Spring 2025
    ## 3046 12-Feb           Create Your Pasta Bowl MEAT   133   Italian Spring 2025
    ## 3047 12-Feb            Create Your Pasta Bowl VEG    22   Italian Spring 2025
    ## 3048 12-Feb                          Pizza Cheese    25   Italian Spring 2025
    ## 3049 12-Feb                        Add Extra Meat    39   Italian Spring 2025
    ## 3050 12-Feb                   Pizza with Toppings    17   Italian Spring 2025
    ## 3051 12-Feb                     Burrito Breakfast    85 Breakfast Spring 2025
    ## 3052 12-Feb                   Small French Omelet    62 Breakfast Spring 2025
    ## 3053 12-Feb                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3054 12-Feb                             Add Bacon    29 Breakfast Spring 2025
    ## 3055 12-Feb                              Two Eggs    15 Breakfast Spring 2025
    ## 3056 12-Feb                        Pancake Single     4 Breakfast Spring 2025
    ## 3057 12-Feb                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 3058 12-Feb                                 Toast     2 Breakfast Spring 2025
    ## 3059 12-Feb                      Burrito Bowl BYO   116   Mexican Spring 2025
    ## 3060 12-Feb                           Single Taco     7   Mexican Spring 2025
    ## 3061 12-Feb                        Side Guacamole     4   Mexican Spring 2025
    ## 3062 12-Feb           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 3063 12-Feb            LTO Spicy Chicken Sandwich    33 Grab N Go Spring 2025
    ## 3064 12-Feb Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3065 12-Feb   Egg Cheese Bacon Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 3066 12-Feb                            Soup 12 oz    48      Soup Spring 2025
    ## 3067 12-Feb                             8 oz Soup    41      Soup Spring 2025
    ## 3068 12-Feb                    Salad by the Pound    49 Salad Bar Spring 2025
    ## 3069 12-Feb                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 3070 13-Feb            Quesadilla Deluxe Trillium   190     Grill Spring 2025
    ## 3071 13-Feb                     Grilled Hamburger    83     Grill Spring 2025
    ## 3072 13-Feb         Burrito Una Mano Trillium BYO    64     Grill Spring 2025
    ## 3073 13-Feb                 Fried Chicken Tenders    76 Grab N Go Spring 2025
    ## 3074 13-Feb                          French Fries   123     Grill Spring 2025
    ## 3075 13-Feb      Trillium Grill Impossible Burger    10     Grill Spring 2025
    ## 3076 13-Feb       Grilled Chicken Breast Sandwich    12     Grill Spring 2025
    ## 3077 13-Feb                  Seared Salmon Burger    11     Grill Spring 2025
    ## 3078 13-Feb                     Quesadilla Cheese    11     Grill Spring 2025
    ## 3079 13-Feb                    Sweet Potato Fries    27     Grill Spring 2025
    ## 3080 13-Feb                      Side Potato Tots    17 Grab N Go Spring 2025
    ## 3081 13-Feb                    ADD Chicken Breast     7     Grill Spring 2025
    ## 3082 13-Feb                     Black Bean Burger     3     Grill Spring 2025
    ## 3083 13-Feb                          + Beef Patty     3     Grill Spring 2025
    ## 3084 13-Feb                           Add Egg .99     7     Grill Spring 2025
    ## 3085 13-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3086 13-Feb                            ADD Cheese     6     Grill Spring 2025
    ## 3087 13-Feb                     1 Entree + 1 Side   185     Asian Spring 2025
    ## 3088 13-Feb                     1 Entree + 2 Side    71     Asian Spring 2025
    ## 3089 13-Feb                    Bowl Ramen Chicken    66     Asian Spring 2025
    ## 3090 13-Feb                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 3091 13-Feb                       Bowl Ramen Tofu    21     Asian Spring 2025
    ## 3092 13-Feb                          1 Wok Entree     9     Asian Spring 2025
    ## 3093 13-Feb              Side White or Brown Rice     8     Asian Spring 2025
    ## 3094 13-Feb               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 3095 13-Feb                       Side Vegetables     2     Asian Spring 2025
    ## 3096 13-Feb                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3097 13-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3098 13-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 3099 13-Feb                     Burrito Breakfast    83 Breakfast Spring 2025
    ## 3100 13-Feb                   Small French Omelet    58 Breakfast Spring 2025
    ## 3101 13-Feb                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 3102 13-Feb                             Add Bacon    26 Breakfast Spring 2025
    ## 3103 13-Feb                              Two Eggs     7 Breakfast Spring 2025
    ## 3104 13-Feb                        Pancake Single     4 Breakfast Spring 2025
    ## 3105 13-Feb                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 3106 13-Feb                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3107 13-Feb                                 Toast     1 Breakfast Spring 2025
    ## 3108 13-Feb                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 3109 13-Feb           Create Your Pasta Bowl MEAT    81   Italian Spring 2025
    ## 3110 13-Feb            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 3111 13-Feb                   Pizza with Toppings    30   Italian Spring 2025
    ## 3112 13-Feb                          Pizza Cheese    17   Italian Spring 2025
    ## 3113 13-Feb                        Add Extra Meat    19   Italian Spring 2025
    ## 3114 13-Feb                      Burrito Bowl BYO    73   Mexican Spring 2025
    ## 3115 13-Feb                           Single Taco     3   Mexican Spring 2025
    ## 3116 13-Feb                        Side Guacamole     1   Mexican Spring 2025
    ## 3117 13-Feb           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 3118 13-Feb            LTO Spicy Chicken Sandwich    35 Grab N Go Spring 2025
    ## 3119 13-Feb Egg Cheese Sausage Breakfast Sandwich    28 Grab N Go Spring 2025
    ## 3120 13-Feb   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 3121 13-Feb                      LTO Meatball Sub    12 Grab N Go Spring 2025
    ## 3122 13-Feb                 Burrito Breakfast G&G     1 Grab N Go Spring 2025
    ## 3123 13-Feb                            Soup 12 oz    52      Soup Spring 2025
    ## 3124 13-Feb                             8 oz Soup    42      Soup Spring 2025
    ## 3125 13-Feb                    Salad by the Pound    33 Salad Bar Spring 2025
    ## 3126 13-Feb                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 3127 14-Feb            Quesadilla Deluxe Trillium   100     Grill Spring 2025
    ## 3128 14-Feb                     Grilled Hamburger    72     Grill Spring 2025
    ## 3129 14-Feb                 Fried Chicken Tenders    53 Grab N Go Spring 2025
    ## 3130 14-Feb                          French Fries   106     Grill Spring 2025
    ## 3131 14-Feb         Burrito Una Mano Trillium BYO    33     Grill Spring 2025
    ## 3132 14-Feb                     Quesadilla Cheese     8     Grill Spring 2025
    ## 3133 14-Feb                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 3134 14-Feb       Grilled Chicken Breast Sandwich     5     Grill Spring 2025
    ## 3135 14-Feb                  Seared Salmon Burger     5     Grill Spring 2025
    ## 3136 14-Feb      Trillium Grill Impossible Burger     4     Grill Spring 2025
    ## 3137 14-Feb                     Black Bean Burger     2     Grill Spring 2025
    ## 3138 14-Feb                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 3139 14-Feb                           Add Egg .99     2     Grill Spring 2025
    ## 3140 14-Feb                     1 Entree + 1 Side    90     Asian Spring 2025
    ## 3141 14-Feb                     1 Entree + 2 Side    46     Asian Spring 2025
    ## 3142 14-Feb                    Bowl Ramen Chicken    42     Asian Spring 2025
    ## 3143 14-Feb                   2 Entrees + 2 Sides    12     Asian Spring 2025
    ## 3144 14-Feb                       Bowl Ramen Tofu    15     Asian Spring 2025
    ## 3145 14-Feb                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3146 14-Feb           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3147 14-Feb               Side Vegetarian Lo Mein     2     Asian Spring 2025
    ## 3148 14-Feb                          1 Wok Entree     1     Asian Spring 2025
    ## 3149 14-Feb              Side White or Brown Rice     1     Asian Spring 2025
    ## 3150 14-Feb                     Burrito Breakfast    72 Breakfast Spring 2025
    ## 3151 14-Feb                   Small French Omelet    29 Breakfast Spring 2025
    ## 3152 14-Feb                  Grand Slam Breakfast     5 Breakfast Spring 2025
    ## 3153 14-Feb                             Add Bacon    18 Breakfast Spring 2025
    ## 3154 14-Feb                              Two Eggs    14 Breakfast Spring 2025
    ## 3155 14-Feb                        Pancake Single     5 Breakfast Spring 2025
    ## 3156 14-Feb                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 3157 14-Feb                                 Toast     1 Breakfast Spring 2025
    ## 3158 14-Feb           Create Your Pasta Bowl MEAT    34   Italian Spring 2025
    ## 3159 14-Feb            Create Your Pasta Bowl VEG    16   Italian Spring 2025
    ## 3160 14-Feb                   Pizza with Toppings    16   Italian Spring 2025
    ## 3161 14-Feb                          Pizza Cheese    13   Italian Spring 2025
    ## 3162 14-Feb                        Add Extra Meat    14   Italian Spring 2025
    ## 3163 14-Feb            LTO Spicy Chicken Sandwich    22 Grab N Go Spring 2025
    ## 3164 14-Feb Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3165 14-Feb   Egg Cheese Bacon Breakfast Sandwich    17 Grab N Go Spring 2025
    ## 3166 14-Feb                      Burrito Bowl BYO    34   Mexican Spring 2025
    ## 3167 14-Feb                           Single Taco     3   Mexican Spring 2025
    ## 3168 14-Feb                        Side Guacamole     2   Mexican Spring 2025
    ## 3169 14-Feb                             8 oz Soup    23      Soup Spring 2025
    ## 3170 14-Feb                            Soup 12 oz    17      Soup Spring 2025
    ## 3171 14-Feb                    Salad by the Pound    20 Salad Bar Spring 2025
    ## 3172 14-Feb                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 3173 19-Feb            Quesadilla Deluxe Trillium   181     Grill Spring 2025
    ## 3174 19-Feb                     Grilled Hamburger    84     Grill Spring 2025
    ## 3175 19-Feb                 Fried Chicken Tenders    71 Grab N Go Spring 2025
    ## 3176 19-Feb         Burrito Una Mano Trillium BYO    50     Grill Spring 2025
    ## 3177 19-Feb                          French Fries   128     Grill Spring 2025
    ## 3178 19-Feb                     Quesadilla Cheese    20     Grill Spring 2025
    ## 3179 19-Feb       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 3180 19-Feb                          + Beef Patty    19     Grill Spring 2025
    ## 3181 19-Feb                    Sweet Potato Fries    23     Grill Spring 2025
    ## 3182 19-Feb                  Seared Salmon Burger     5     Grill Spring 2025
    ## 3183 19-Feb                      Side Potato Tots    11 Grab N Go Spring 2025
    ## 3184 19-Feb      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 3185 19-Feb                     Black Bean Burger     1     Grill Spring 2025
    ## 3186 19-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3187 19-Feb                            ADD Cheese    10     Grill Spring 2025
    ## 3188 19-Feb                    ADD Chicken Breast     1     Grill Spring 2025
    ## 3189 19-Feb                           Add Egg .99     3     Grill Spring 2025
    ## 3190 19-Feb                     1 Entree + 1 Side   200     Asian Spring 2025
    ## 3191 19-Feb                     1 Entree + 2 Side    78     Asian Spring 2025
    ## 3192 19-Feb                    Bowl Ramen Chicken    80     Asian Spring 2025
    ## 3193 19-Feb                   2 Entrees + 2 Sides    21     Asian Spring 2025
    ## 3194 19-Feb                       Bowl Ramen Tofu    15     Asian Spring 2025
    ## 3195 19-Feb                          1 Wok Entree    10     Asian Spring 2025
    ## 3196 19-Feb               Side Vegetarian Lo Mein     4     Asian Spring 2025
    ## 3197 19-Feb              Side White or Brown Rice     7     Asian Spring 2025
    ## 3198 19-Feb           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 3199 19-Feb                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3200 19-Feb                       Side Vegetables     2     Asian Spring 2025
    ## 3201 19-Feb       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 3202 19-Feb           Create Your Pasta Bowl MEAT   104   Italian Spring 2025
    ## 3203 19-Feb            Create Your Pasta Bowl VEG    28   Italian Spring 2025
    ## 3204 19-Feb                   Pizza with Toppings    20   Italian Spring 2025
    ## 3205 19-Feb                          Pizza Cheese    24   Italian Spring 2025
    ## 3206 19-Feb                        Add Extra Meat    27   Italian Spring 2025
    ## 3207 19-Feb                     Burrito Breakfast    76 Breakfast Spring 2025
    ## 3208 19-Feb                   Small French Omelet    58 Breakfast Spring 2025
    ## 3209 19-Feb                  Grand Slam Breakfast     7 Breakfast Spring 2025
    ## 3210 19-Feb                             Add Bacon    27 Breakfast Spring 2025
    ## 3211 19-Feb                              Two Eggs    11 Breakfast Spring 2025
    ## 3212 19-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 3213 19-Feb                        Pancake Single     1 Breakfast Spring 2025
    ## 3214 19-Feb                                 Toast     1 Breakfast Spring 2025
    ## 3215 19-Feb                      Burrito Bowl BYO   103   Mexican Spring 2025
    ## 3216 19-Feb                           Single Taco     2   Mexican Spring 2025
    ## 3217 19-Feb                        Side Guacamole     2   Mexican Spring 2025
    ## 3218 19-Feb                            Soup 12 oz    47      Soup Spring 2025
    ## 3219 19-Feb                             8 oz Soup    42      Soup Spring 2025
    ## 3220 19-Feb            LTO Spicy Chicken Sandwich    24 Grab N Go Spring 2025
    ## 3221 19-Feb   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 3222 19-Feb Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3223 19-Feb                    Salad by the Pound    47 Salad Bar Spring 2025
    ## 3224 19-Feb                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 3225 19-Feb                          LTO Sandwich     5      Deli Spring 2025
    ## 3226 20-Feb            Quesadilla Deluxe Trillium   172     Grill Spring 2025
    ## 3227 20-Feb                     Grilled Hamburger   102     Grill Spring 2025
    ## 3228 20-Feb                 Fried Chicken Tenders    82 Grab N Go Spring 2025
    ## 3229 20-Feb         Burrito Una Mano Trillium BYO    54     Grill Spring 2025
    ## 3230 20-Feb                          French Fries   133     Grill Spring 2025
    ## 3231 20-Feb       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 3232 20-Feb                    Sweet Potato Fries    38     Grill Spring 2025
    ## 3233 20-Feb      Trillium Grill Impossible Burger    10     Grill Spring 2025
    ## 3234 20-Feb                     Quesadilla Cheese    10     Grill Spring 2025
    ## 3235 20-Feb                          + Beef Patty    19     Grill Spring 2025
    ## 3236 20-Feb                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 3237 20-Feb                  Seared Salmon Burger     5     Grill Spring 2025
    ## 3238 20-Feb                     Black Bean Burger     4     Grill Spring 2025
    ## 3239 20-Feb                    ADD Chicken Breast     3     Grill Spring 2025
    ## 3240 20-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3241 20-Feb                            ADD Cheese     8     Grill Spring 2025
    ## 3242 20-Feb                           Add Egg .99     1     Grill Spring 2025
    ## 3243 20-Feb                     1 Entree + 1 Side   178     Asian Spring 2025
    ## 3244 20-Feb                     1 Entree + 2 Side    70     Asian Spring 2025
    ## 3245 20-Feb                    Bowl Ramen Chicken    62     Asian Spring 2025
    ## 3246 20-Feb                   2 Entrees + 2 Sides    23     Asian Spring 2025
    ## 3247 20-Feb                       Bowl Ramen Tofu    15     Asian Spring 2025
    ## 3248 20-Feb               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 3249 20-Feb                          1 Wok Entree     3     Asian Spring 2025
    ## 3250 20-Feb              Side White or Brown Rice     4     Asian Spring 2025
    ## 3251 20-Feb       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 3252 20-Feb                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3253 20-Feb           Create Your Pasta Bowl MEAT    93   Italian Spring 2025
    ## 3254 20-Feb            Create Your Pasta Bowl VEG    27   Italian Spring 2025
    ## 3255 20-Feb                   Pizza with Toppings    32   Italian Spring 2025
    ## 3256 20-Feb                          Pizza Cheese    19   Italian Spring 2025
    ## 3257 20-Feb                        Add Extra Meat    24   Italian Spring 2025
    ## 3258 20-Feb                     Burrito Breakfast    80 Breakfast Spring 2025
    ## 3259 20-Feb                   Small French Omelet    65 Breakfast Spring 2025
    ## 3260 20-Feb                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3261 20-Feb                             Add Bacon    38 Breakfast Spring 2025
    ## 3262 20-Feb                              Two Eggs    17 Breakfast Spring 2025
    ## 3263 20-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 3264 20-Feb                        Pancake Single     2 Breakfast Spring 2025
    ## 3265 20-Feb                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3266 20-Feb                      Burrito Bowl BYO    98   Mexican Spring 2025
    ## 3267 20-Feb                           Single Taco     5   Mexican Spring 2025
    ## 3268 20-Feb                        Side Guacamole     1   Mexican Spring 2025
    ## 3269 20-Feb                       Side Sour Cream     1   Mexican Spring 2025
    ## 3270 20-Feb            LTO Spicy Chicken Sandwich    27 Grab N Go Spring 2025
    ## 3271 20-Feb                      LTO Meatball Sub    25 Grab N Go Spring 2025
    ## 3272 20-Feb Egg Cheese Sausage Breakfast Sandwich    31 Grab N Go Spring 2025
    ## 3273 20-Feb   Egg Cheese Bacon Breakfast Sandwich    14 Grab N Go Spring 2025
    ## 3274 20-Feb                            Soup 12 oz    42      Soup Spring 2025
    ## 3275 20-Feb                             8 oz Soup    37      Soup Spring 2025
    ## 3276 20-Feb                    Salad by the Pound    34 Salad Bar Spring 2025
    ## 3277 20-Feb                Add Extra Protein 3.99     7 Salad Bar Spring 2025
    ## 3278 21-Feb            Quesadilla Deluxe Trillium   134     Grill Spring 2025
    ## 3279 21-Feb                     Grilled Hamburger    73     Grill Spring 2025
    ## 3280 21-Feb         Burrito Una Mano Trillium BYO    55     Grill Spring 2025
    ## 3281 21-Feb                 Fried Chicken Tenders    56 Grab N Go Spring 2025
    ## 3282 21-Feb                          French Fries   105     Grill Spring 2025
    ## 3283 21-Feb       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 3284 21-Feb                     Quesadilla Cheese    11     Grill Spring 2025
    ## 3285 21-Feb                    Sweet Potato Fries    26     Grill Spring 2025
    ## 3286 21-Feb                  Seared Salmon Burger     8     Grill Spring 2025
    ## 3287 21-Feb                          + Beef Patty    12     Grill Spring 2025
    ## 3288 21-Feb                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 3289 21-Feb      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 3290 21-Feb                     Black Bean Burger     2     Grill Spring 2025
    ## 3291 21-Feb                            ADD Cheese     5     Grill Spring 2025
    ## 3292 21-Feb                           Add Egg .99     1     Grill Spring 2025
    ## 3293 21-Feb                     1 Entree + 1 Side   133     Asian Spring 2025
    ## 3294 21-Feb                     1 Entree + 2 Side    55     Asian Spring 2025
    ## 3295 21-Feb                    Bowl Ramen Chicken    58     Asian Spring 2025
    ## 3296 21-Feb                   2 Entrees + 2 Sides    18     Asian Spring 2025
    ## 3297 21-Feb                       Bowl Ramen Tofu    14     Asian Spring 2025
    ## 3298 21-Feb                          1 Wok Entree     6     Asian Spring 2025
    ## 3299 21-Feb               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 3300 21-Feb              Side White or Brown Rice     5     Asian Spring 2025
    ## 3301 21-Feb                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3302 21-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3303 21-Feb           Create Your Pasta Bowl MEAT    78   Italian Spring 2025
    ## 3304 21-Feb                   Pizza with Toppings    28   Italian Spring 2025
    ## 3305 21-Feb            Create Your Pasta Bowl VEG    16   Italian Spring 2025
    ## 3306 21-Feb                          Pizza Cheese    15   Italian Spring 2025
    ## 3307 21-Feb                        Add Extra Meat    12   Italian Spring 2025
    ## 3308 21-Feb                     Burrito Breakfast    66 Breakfast Spring 2025
    ## 3309 21-Feb                   Small French Omelet    42 Breakfast Spring 2025
    ## 3310 21-Feb                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 3311 21-Feb                             Add Bacon    18 Breakfast Spring 2025
    ## 3312 21-Feb                              Two Eggs     7 Breakfast Spring 2025
    ## 3313 21-Feb                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 3314 21-Feb                        Pancake Single     1 Breakfast Spring 2025
    ## 3315 21-Feb                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 3316 21-Feb                      Burrito Bowl BYO    66   Mexican Spring 2025
    ## 3317 21-Feb                           Single Taco     4   Mexican Spring 2025
    ## 3318 21-Feb                        Side Guacamole     3   Mexican Spring 2025
    ## 3319 21-Feb            LTO Spicy Chicken Sandwich    25 Grab N Go Spring 2025
    ## 3320 21-Feb   Egg Cheese Bacon Breakfast Sandwich    25 Grab N Go Spring 2025
    ## 3321 21-Feb Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 3322 21-Feb                            Soup 12 oz    37      Soup Spring 2025
    ## 3323 21-Feb                             8 oz Soup    39      Soup Spring 2025
    ## 3324 21-Feb                    Salad by the Pound    32 Salad Bar Spring 2025
    ## 3325 21-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 3326 24-Feb            Quesadilla Deluxe Trillium   186     Grill Spring 2025
    ## 3327 24-Feb                     Grilled Hamburger    95     Grill Spring 2025
    ## 3328 24-Feb                 Fried Chicken Tenders    85 Grab N Go Spring 2025
    ## 3329 24-Feb         Burrito Una Mano Trillium BYO    55     Grill Spring 2025
    ## 3330 24-Feb                          French Fries   132     Grill Spring 2025
    ## 3331 24-Feb                     Quesadilla Cheese    16     Grill Spring 2025
    ## 3332 24-Feb       Grilled Chicken Breast Sandwich    15     Grill Spring 2025
    ## 3333 24-Feb                  Seared Salmon Burger    14     Grill Spring 2025
    ## 3334 24-Feb                          + Beef Patty    25     Grill Spring 2025
    ## 3335 24-Feb                    Sweet Potato Fries    29     Grill Spring 2025
    ## 3336 24-Feb      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 3337 24-Feb                      Side Potato Tots    17 Grab N Go Spring 2025
    ## 3338 24-Feb                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 3339 24-Feb                     Black Bean Burger     1     Grill Spring 2025
    ## 3340 24-Feb                    ADD Chicken Breast     2     Grill Spring 2025
    ## 3341 24-Feb                            ADD Cheese     4     Grill Spring 2025
    ## 3342 24-Feb                           Add Egg .99     1     Grill Spring 2025
    ## 3343 24-Feb                     1 Entree + 1 Side   235     Asian Spring 2025
    ## 3344 24-Feb                     1 Entree + 2 Side    82     Asian Spring 2025
    ## 3345 24-Feb                    Bowl Ramen Chicken    79     Asian Spring 2025
    ## 3346 24-Feb                   2 Entrees + 2 Sides    22     Asian Spring 2025
    ## 3347 24-Feb                       Bowl Ramen Tofu    14     Asian Spring 2025
    ## 3348 24-Feb                          1 Wok Entree     8     Asian Spring 2025
    ## 3349 24-Feb               Side Vegetarian Lo Mein     4     Asian Spring 2025
    ## 3350 24-Feb              Side White or Brown Rice     6     Asian Spring 2025
    ## 3351 24-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3352 24-Feb       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 3353 24-Feb           Create Your Pasta Bowl MEAT   130   Italian Spring 2025
    ## 3354 24-Feb            Create Your Pasta Bowl VEG    22   Italian Spring 2025
    ## 3355 24-Feb                   Pizza with Toppings    32   Italian Spring 2025
    ## 3356 24-Feb                        Add Extra Meat    31   Italian Spring 2025
    ## 3357 24-Feb                          Pizza Cheese    15   Italian Spring 2025
    ## 3358 24-Feb                     Burrito Breakfast    91 Breakfast Spring 2025
    ## 3359 24-Feb                   Small French Omelet    60 Breakfast Spring 2025
    ## 3360 24-Feb                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 3361 24-Feb                             Add Bacon    25 Breakfast Spring 2025
    ## 3362 24-Feb                              Two Eggs    14 Breakfast Spring 2025
    ## 3363 24-Feb                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 3364 24-Feb                        Pancake Single     2 Breakfast Spring 2025
    ## 3365 24-Feb                        2 Slices Toast     1 Breakfast Spring 2025
    ## 3366 24-Feb                                 Toast     1 Breakfast Spring 2025
    ## 3367 24-Feb                      Burrito Bowl BYO   120   Mexican Spring 2025
    ## 3368 24-Feb                           Single Taco     7   Mexican Spring 2025
    ## 3369 24-Feb                        Side Guacamole     3   Mexican Spring 2025
    ## 3370 24-Feb           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 3371 24-Feb                    Salad by the Pound    59 Salad Bar Spring 2025
    ## 3372 24-Feb                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 3373 24-Feb            LTO Spicy Chicken Sandwich    29 Grab N Go Spring 2025
    ## 3374 24-Feb   Egg Cheese Bacon Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3375 24-Feb Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 3376 24-Feb                             8 oz Soup    46      Soup Spring 2025
    ## 3377 24-Feb                            Soup 12 oz    40      Soup Spring 2025
    ## 3378 24-Feb                          LTO Sandwich     2      Deli Spring 2025
    ## 3379 25-Feb            Quesadilla Deluxe Trillium   186     Grill Spring 2025
    ## 3380 25-Feb                     Grilled Hamburger   102     Grill Spring 2025
    ## 3381 25-Feb                 Fried Chicken Tenders    85 Grab N Go Spring 2025
    ## 3382 25-Feb         Burrito Una Mano Trillium BYO    64     Grill Spring 2025
    ## 3383 25-Feb                          French Fries   142     Grill Spring 2025
    ## 3384 25-Feb                     Quesadilla Cheese    24     Grill Spring 2025
    ## 3385 25-Feb       Grilled Chicken Breast Sandwich    20     Grill Spring 2025
    ## 3386 25-Feb                    Sweet Potato Fries    37     Grill Spring 2025
    ## 3387 25-Feb      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 3388 25-Feb                  Seared Salmon Burger     6     Grill Spring 2025
    ## 3389 25-Feb                     Black Bean Burger     4     Grill Spring 2025
    ## 3390 25-Feb                      Side Potato Tots     9 Grab N Go Spring 2025
    ## 3391 25-Feb                          + Beef Patty     7     Grill Spring 2025
    ## 3392 25-Feb           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 3393 25-Feb                    ADD Chicken Breast     1     Grill Spring 2025
    ## 3394 25-Feb                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 3395 25-Feb                     1 Entree + 1 Side   181     Asian Spring 2025
    ## 3396 25-Feb                    Bowl Ramen Chicken   101     Asian Spring 2025
    ## 3397 25-Feb                     1 Entree + 2 Side    71     Asian Spring 2025
    ## 3398 25-Feb                   2 Entrees + 2 Sides    37     Asian Spring 2025
    ## 3399 25-Feb                       Bowl Ramen Tofu    24     Asian Spring 2025
    ## 3400 25-Feb                          1 Wok Entree     4     Asian Spring 2025
    ## 3401 25-Feb               Side Vegetarian Lo Mein     5     Asian Spring 2025
    ## 3402 25-Feb       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 3403 25-Feb              Side White or Brown Rice     3     Asian Spring 2025
    ## 3404 25-Feb           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3405 25-Feb           Create Your Pasta Bowl MEAT   105   Italian Spring 2025
    ## 3406 25-Feb                   Pizza with Toppings    42   Italian Spring 2025
    ## 3407 25-Feb            Create Your Pasta Bowl VEG    16   Italian Spring 2025
    ## 3408 25-Feb                          Pizza Cheese    23   Italian Spring 2025
    ## 3409 25-Feb                        Add Extra Meat    24   Italian Spring 2025
    ## 3410 25-Feb              Side Bread Pasta Station     1   Italian Spring 2025
    ## 3411 25-Feb                     Burrito Breakfast    96 Breakfast Spring 2025
    ## 3412 25-Feb                   Small French Omelet    59 Breakfast Spring 2025
    ## 3413 25-Feb                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3414 25-Feb                             Add Bacon    38 Breakfast Spring 2025
    ## 3415 25-Feb                              Two Eggs    11 Breakfast Spring 2025
    ## 3416 25-Feb                        Pancake Single     4 Breakfast Spring 2025
    ## 3417 25-Feb                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 3418 25-Feb                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3419 25-Feb                      Burrito Bowl BYO    90   Mexican Spring 2025
    ## 3420 25-Feb                           Single Taco     9   Mexican Spring 2025
    ## 3421 25-Feb                        Side Guacamole     8   Mexican Spring 2025
    ## 3422 25-Feb           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 3423 25-Feb                            Side Salsa     1   Mexican Spring 2025
    ## 3424 25-Feb            LTO Spicy Chicken Sandwich    26 Grab N Go Spring 2025
    ## 3425 25-Feb Egg Cheese Sausage Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 3426 25-Feb   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 3427 25-Feb                      LTO Meatball Sub    12 Grab N Go Spring 2025
    ## 3428 25-Feb                    Salad by the Pound    52 Salad Bar Spring 2025
    ## 3429 25-Feb                            Soup 12 oz    63      Soup Spring 2025
    ## 3430 25-Feb                             8 oz Soup    32      Soup Spring 2025
    ## 3431 25-Feb                          LTO Sandwich     8      Deli Spring 2025
    ## 3432 26-Feb            Quesadilla Deluxe Trillium   177     Grill Spring 2025
    ## 3433 26-Feb                     Grilled Hamburger    90     Grill Spring 2025
    ## 3434 26-Feb                 Fried Chicken Tenders    99 Grab N Go Spring 2025
    ## 3435 26-Feb         Burrito Una Mano Trillium BYO    69     Grill Spring 2025
    ## 3436 26-Feb                          French Fries   143     Grill Spring 2025
    ## 3437 26-Feb       Grilled Chicken Breast Sandwich    28     Grill Spring 2025
    ## 3438 26-Feb                     Quesadilla Cheese    19     Grill Spring 2025
    ## 3439 26-Feb                  Seared Salmon Burger    15     Grill Spring 2025
    ## 3440 26-Feb                    Sweet Potato Fries    26     Grill Spring 2025
    ## 3441 26-Feb                          + Beef Patty    19     Grill Spring 2025
    ## 3442 26-Feb                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 3443 26-Feb      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 3444 26-Feb                     Black Bean Burger     1     Grill Spring 2025
    ## 3445 26-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3446 26-Feb                            ADD Cheese     8     Grill Spring 2025
    ## 3447 26-Feb                    ADD Chicken Breast     1     Grill Spring 2025
    ## 3448 26-Feb                           Add Egg .99     1     Grill Spring 2025
    ## 3449 26-Feb                     1 Entree + 1 Side   217     Asian Spring 2025
    ## 3450 26-Feb                     1 Entree + 2 Side    83     Asian Spring 2025
    ## 3451 26-Feb                    Bowl Ramen Chicken    63     Asian Spring 2025
    ## 3452 26-Feb                   2 Entrees + 2 Sides    23     Asian Spring 2025
    ## 3453 26-Feb                       Bowl Ramen Tofu    12     Asian Spring 2025
    ## 3454 26-Feb                          1 Wok Entree     5     Asian Spring 2025
    ## 3455 26-Feb              Side White or Brown Rice     5     Asian Spring 2025
    ## 3456 26-Feb                       Side Vegetables     2     Asian Spring 2025
    ## 3457 26-Feb               Side Vegetarian Lo Mein     2     Asian Spring 2025
    ## 3458 26-Feb                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3459 26-Feb           Create Your Pasta Bowl MEAT   136   Italian Spring 2025
    ## 3460 26-Feb                   Pizza with Toppings    34   Italian Spring 2025
    ## 3461 26-Feb            Create Your Pasta Bowl VEG    19   Italian Spring 2025
    ## 3462 26-Feb                          Pizza Cheese    19   Italian Spring 2025
    ## 3463 26-Feb                        Add Extra Meat    31   Italian Spring 2025
    ## 3464 26-Feb              Side Bread Pasta Station     2   Italian Spring 2025
    ## 3465 26-Feb                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 3466 26-Feb                   Small French Omelet    63 Breakfast Spring 2025
    ## 3467 26-Feb                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 3468 26-Feb                             Add Bacon    41 Breakfast Spring 2025
    ## 3469 26-Feb                              Two Eggs    18 Breakfast Spring 2025
    ## 3470 26-Feb                   Trillium Home Fries     6 Breakfast Spring 2025
    ## 3471 26-Feb                        Pancake Single     4 Breakfast Spring 2025
    ## 3472 26-Feb                        2 Slices Toast     4 Breakfast Spring 2025
    ## 3473 26-Feb                              PC Jelly     3 Breakfast Spring 2025
    ## 3474 26-Feb                      PC Peanut Butter     2 Breakfast Spring 2025
    ## 3475 26-Feb                      Burrito Bowl BYO   104   Mexican Spring 2025
    ## 3476 26-Feb                           Single Taco     2   Mexican Spring 2025
    ## 3477 26-Feb                        Side Guacamole     2   Mexican Spring 2025
    ## 3478 26-Feb                            Soup 12 oz    47      Soup Spring 2025
    ## 3479 26-Feb                             8 oz Soup    44      Soup Spring 2025
    ## 3480 26-Feb            LTO Spicy Chicken Sandwich    28 Grab N Go Spring 2025
    ## 3481 26-Feb Egg Cheese Sausage Breakfast Sandwich    32 Grab N Go Spring 2025
    ## 3482 26-Feb   Egg Cheese Bacon Breakfast Sandwich    14 Grab N Go Spring 2025
    ## 3483 26-Feb                    Salad by the Pound    52 Salad Bar Spring 2025
    ## 3484 26-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 3485 27-Feb            Quesadilla Deluxe Trillium   198     Grill Spring 2025
    ## 3486 27-Feb                     Grilled Hamburger   105     Grill Spring 2025
    ## 3487 27-Feb                 Fried Chicken Tenders   108 Grab N Go Spring 2025
    ## 3488 27-Feb         Burrito Una Mano Trillium BYO    65     Grill Spring 2025
    ## 3489 27-Feb                          French Fries   166     Grill Spring 2025
    ## 3490 27-Feb                     Quesadilla Cheese    23     Grill Spring 2025
    ## 3491 27-Feb       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 3492 27-Feb                  Seared Salmon Burger    13     Grill Spring 2025
    ## 3493 27-Feb                    Sweet Potato Fries    35     Grill Spring 2025
    ## 3494 27-Feb      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 3495 27-Feb                      Side Potato Tots    25 Grab N Go Spring 2025
    ## 3496 27-Feb                          + Beef Patty    12     Grill Spring 2025
    ## 3497 27-Feb                     Black Bean Burger     2     Grill Spring 2025
    ## 3498 27-Feb                   Add Sausage 2 Patty     6     Grill Spring 2025
    ## 3499 27-Feb                    ADD Chicken Breast     2     Grill Spring 2025
    ## 3500 27-Feb           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 3501 27-Feb                            ADD Cheese     6     Grill Spring 2025
    ## 3502 27-Feb                           Add Egg .99     1     Grill Spring 2025
    ## 3503 27-Feb                     1 Entree + 1 Side   212     Asian Spring 2025
    ## 3504 27-Feb                     1 Entree + 2 Side    82     Asian Spring 2025
    ## 3505 27-Feb                    Bowl Ramen Chicken    85     Asian Spring 2025
    ## 3506 27-Feb                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 3507 27-Feb                       Bowl Ramen Tofu    18     Asian Spring 2025
    ## 3508 27-Feb               Side Vegetarian Lo Mein     8     Asian Spring 2025
    ## 3509 27-Feb                          1 Wok Entree     5     Asian Spring 2025
    ## 3510 27-Feb              Side White or Brown Rice     5     Asian Spring 2025
    ## 3511 27-Feb                Side Fried Spring Roll     2     Asian Spring 2025
    ## 3512 27-Feb           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3513 27-Feb           Create Your Pasta Bowl MEAT    89   Italian Spring 2025
    ## 3514 27-Feb            Create Your Pasta Bowl VEG    30   Italian Spring 2025
    ## 3515 27-Feb                   Pizza with Toppings    36   Italian Spring 2025
    ## 3516 27-Feb                          Pizza Cheese    24   Italian Spring 2025
    ## 3517 27-Feb                        Add Extra Meat    33   Italian Spring 2025
    ## 3518 27-Feb                     Burrito Breakfast    84 Breakfast Spring 2025
    ## 3519 27-Feb                   Small French Omelet    58 Breakfast Spring 2025
    ## 3520 27-Feb                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 3521 27-Feb                             Add Bacon    33 Breakfast Spring 2025
    ## 3522 27-Feb                              Two Eggs    13 Breakfast Spring 2025
    ## 3523 27-Feb                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 3524 27-Feb                        Pancake Single     2 Breakfast Spring 2025
    ## 3525 27-Feb                                 Toast     2 Breakfast Spring 2025
    ## 3526 27-Feb                        2 Slices Toast     1 Breakfast Spring 2025
    ## 3527 27-Feb                      Burrito Bowl BYO   103   Mexican Spring 2025
    ## 3528 27-Feb                           Single Taco     6   Mexican Spring 2025
    ## 3529 27-Feb                        Side Guacamole     5   Mexican Spring 2025
    ## 3530 27-Feb           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 3531 27-Feb            LTO Spicy Chicken Sandwich    32 Grab N Go Spring 2025
    ## 3532 27-Feb                      LTO Meatball Sub    15 Grab N Go Spring 2025
    ## 3533 27-Feb Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3534 27-Feb   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 3535 27-Feb                            Soup 12 oz    60      Soup Spring 2025
    ## 3536 27-Feb                             8 oz Soup    36      Soup Spring 2025
    ## 3537 27-Feb                    Salad by the Pound    52 Salad Bar Spring 2025
    ## 3538 28-Feb            Quesadilla Deluxe Trillium   147     Grill Spring 2025
    ## 3539 28-Feb                     Grilled Hamburger    72     Grill Spring 2025
    ## 3540 28-Feb                 Fried Chicken Tenders    63 Grab N Go Spring 2025
    ## 3541 28-Feb         Burrito Una Mano Trillium BYO    44     Grill Spring 2025
    ## 3542 28-Feb                          French Fries    95     Grill Spring 2025
    ## 3543 28-Feb       Grilled Chicken Breast Sandwich    14     Grill Spring 2025
    ## 3544 28-Feb                    Sweet Potato Fries    27     Grill Spring 2025
    ## 3545 28-Feb                  Seared Salmon Burger     9     Grill Spring 2025
    ## 3546 28-Feb                     Quesadilla Cheese     8     Grill Spring 2025
    ## 3547 28-Feb                          + Beef Patty    12     Grill Spring 2025
    ## 3548 28-Feb                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 3549 28-Feb      Trillium Grill Impossible Burger     2     Grill Spring 2025
    ## 3550 28-Feb                     Black Bean Burger     1     Grill Spring 2025
    ## 3551 28-Feb                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3552 28-Feb                    ADD Chicken Breast     1     Grill Spring 2025
    ## 3553 28-Feb                            ADD Cheese     6     Grill Spring 2025
    ## 3554 28-Feb                     1 Entree + 1 Side   122     Asian Spring 2025
    ## 3555 28-Feb                     1 Entree + 2 Side    47     Asian Spring 2025
    ## 3556 28-Feb                    Bowl Ramen Chicken    49     Asian Spring 2025
    ## 3557 28-Feb                   2 Entrees + 2 Sides    12     Asian Spring 2025
    ## 3558 28-Feb                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 3559 28-Feb                          1 Wok Entree     2     Asian Spring 2025
    ## 3560 28-Feb              Side White or Brown Rice     4     Asian Spring 2025
    ## 3561 28-Feb               Side Vegetarian Lo Mein     2     Asian Spring 2025
    ## 3562 28-Feb                     Burrito Breakfast    88 Breakfast Spring 2025
    ## 3563 28-Feb                   Small French Omelet    59 Breakfast Spring 2025
    ## 3564 28-Feb                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3565 28-Feb                             Add Bacon    20 Breakfast Spring 2025
    ## 3566 28-Feb                   Trillium Home Fries     5 Breakfast Spring 2025
    ## 3567 28-Feb                        Pancake Single     5 Breakfast Spring 2025
    ## 3568 28-Feb                              Two Eggs     5 Breakfast Spring 2025
    ## 3569 28-Feb                                 Toast     3 Breakfast Spring 2025
    ## 3570 28-Feb                        2 Slices Toast     1 Breakfast Spring 2025
    ## 3571 28-Feb           Create Your Pasta Bowl MEAT    84   Italian Spring 2025
    ## 3572 28-Feb                   Pizza with Toppings    23   Italian Spring 2025
    ## 3573 28-Feb            Create Your Pasta Bowl VEG    13   Italian Spring 2025
    ## 3574 28-Feb                          Pizza Cheese    14   Italian Spring 2025
    ## 3575 28-Feb                        Add Extra Meat    21   Italian Spring 2025
    ## 3576 28-Feb              Side Bread Pasta Station     1   Italian Spring 2025
    ## 3577 28-Feb                      Burrito Bowl BYO    55   Mexican Spring 2025
    ## 3578 28-Feb                           Single Taco     3   Mexican Spring 2025
    ## 3579 28-Feb                        Side Guacamole     2   Mexican Spring 2025
    ## 3580 28-Feb           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 3581 28-Feb            LTO Spicy Chicken Sandwich    22 Grab N Go Spring 2025
    ## 3582 28-Feb Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 3583 28-Feb   Egg Cheese Bacon Breakfast Sandwich    18 Grab N Go Spring 2025
    ## 3584 28-Feb                            Soup 12 oz    33      Soup Spring 2025
    ## 3585 28-Feb                             8 oz Soup    33      Soup Spring 2025
    ## 3586 28-Feb                    Salad by the Pound    34 Salad Bar Spring 2025
    ## 3587 28-Feb                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 3588  3-Mar            Quesadilla Deluxe Trillium   152     Grill Spring 2025
    ## 3589  3-Mar                     Grilled Hamburger    95     Grill Spring 2025
    ## 3590  3-Mar         Burrito Una Mano Trillium BYO    74     Grill Spring 2025
    ## 3591  3-Mar                 Fried Chicken Tenders    79 Grab N Go Spring 2025
    ## 3592  3-Mar                          French Fries   136     Grill Spring 2025
    ## 3593  3-Mar                     Quesadilla Cheese    20     Grill Spring 2025
    ## 3594  3-Mar       Grilled Chicken Breast Sandwich    16     Grill Spring 2025
    ## 3595  3-Mar      Trillium Grill Impossible Burger    12     Grill Spring 2025
    ## 3596  3-Mar                    Sweet Potato Fries    34     Grill Spring 2025
    ## 3597  3-Mar                  Seared Salmon Burger     9     Grill Spring 2025
    ## 3598  3-Mar                          + Beef Patty    11     Grill Spring 2025
    ## 3599  3-Mar                      Side Potato Tots     8 Grab N Go Spring 2025
    ## 3600  3-Mar                     Black Bean Burger     2     Grill Spring 2025
    ## 3601  3-Mar                    ADD Chicken Breast     3     Grill Spring 2025
    ## 3602  3-Mar                            ADD Cheese     8     Grill Spring 2025
    ## 3603  3-Mar                           Add Egg .99     1     Grill Spring 2025
    ## 3604  3-Mar                     1 Entree + 1 Side   204     Asian Spring 2025
    ## 3605  3-Mar                    Bowl Ramen Chicken    72     Asian Spring 2025
    ## 3606  3-Mar                     1 Entree + 2 Side    64     Asian Spring 2025
    ## 3607  3-Mar                   2 Entrees + 2 Sides    26     Asian Spring 2025
    ## 3608  3-Mar                       Bowl Ramen Tofu    17     Asian Spring 2025
    ## 3609  3-Mar                          1 Wok Entree     6     Asian Spring 2025
    ## 3610  3-Mar               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 3611  3-Mar                Side Fried Spring Roll     3     Asian Spring 2025
    ## 3612  3-Mar              Side White or Brown Rice     3     Asian Spring 2025
    ## 3613  3-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 3614  3-Mar           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3615  3-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 3616  3-Mar           Create Your Pasta Bowl MEAT   107   Italian Spring 2025
    ## 3617  3-Mar                   Pizza with Toppings    36   Italian Spring 2025
    ## 3618  3-Mar            Create Your Pasta Bowl VEG    13   Italian Spring 2025
    ## 3619  3-Mar                          Pizza Cheese    20   Italian Spring 2025
    ## 3620  3-Mar                        Add Extra Meat    19   Italian Spring 2025
    ## 3621  3-Mar                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 3622  3-Mar                   Small French Omelet    52 Breakfast Spring 2025
    ## 3623  3-Mar                             Add Bacon    38 Breakfast Spring 2025
    ## 3624  3-Mar                  Grand Slam Breakfast     6 Breakfast Spring 2025
    ## 3625  3-Mar                              Two Eggs    11 Breakfast Spring 2025
    ## 3626  3-Mar                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 3627  3-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3628  3-Mar                      Burrito Bowl BYO    94   Mexican Spring 2025
    ## 3629  3-Mar                        Side Guacamole     4   Mexican Spring 2025
    ## 3630  3-Mar                           Single Taco     1   Mexican Spring 2025
    ## 3631  3-Mar            LTO Spicy Chicken Sandwich    30 Grab N Go Spring 2025
    ## 3632  3-Mar   Egg Cheese Bacon Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3633  3-Mar Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3634  3-Mar                    Salad by the Pound    50 Salad Bar Spring 2025
    ## 3635  3-Mar                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 3636  3-Mar                             8 oz Soup    43      Soup Spring 2025
    ## 3637  3-Mar                            Soup 12 oz    30      Soup Spring 2025
    ## 3638  4-Mar            Quesadilla Deluxe Trillium   189     Grill Spring 2025
    ## 3639  4-Mar                     Grilled Hamburger   104     Grill Spring 2025
    ## 3640  4-Mar                 Fried Chicken Tenders   113 Grab N Go Spring 2025
    ## 3641  4-Mar         Burrito Una Mano Trillium BYO    60     Grill Spring 2025
    ## 3642  4-Mar                          French Fries   148     Grill Spring 2025
    ## 3643  4-Mar       Grilled Chicken Breast Sandwich    21     Grill Spring 2025
    ## 3644  4-Mar                     Quesadilla Cheese    17     Grill Spring 2025
    ## 3645  4-Mar                    Sweet Potato Fries    41     Grill Spring 2025
    ## 3646  4-Mar                  Seared Salmon Burger    10     Grill Spring 2025
    ## 3647  4-Mar      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 3648  4-Mar                          + Beef Patty    15     Grill Spring 2025
    ## 3649  4-Mar                      Side Potato Tots    11 Grab N Go Spring 2025
    ## 3650  4-Mar                    ADD Chicken Breast     8     Grill Spring 2025
    ## 3651  4-Mar                     Black Bean Burger     2     Grill Spring 2025
    ## 3652  4-Mar                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 3653  4-Mar                            ADD Cheese    11     Grill Spring 2025
    ## 3654  4-Mar                           Add Egg .99     2     Grill Spring 2025
    ## 3655  4-Mar                     1 Entree + 1 Side   215     Asian Spring 2025
    ## 3656  4-Mar                    Bowl Ramen Chicken    75     Asian Spring 2025
    ## 3657  4-Mar                     1 Entree + 2 Side    59     Asian Spring 2025
    ## 3658  4-Mar                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 3659  4-Mar                       Bowl Ramen Tofu    20     Asian Spring 2025
    ## 3660  4-Mar                          1 Wok Entree    13     Asian Spring 2025
    ## 3661  4-Mar               Side Vegetarian Lo Mein     8     Asian Spring 2025
    ## 3662  4-Mar              Side White or Brown Rice     8     Asian Spring 2025
    ## 3663  4-Mar                Side Fried Spring Roll     2     Asian Spring 2025
    ## 3664  4-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3665  4-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 3666  4-Mar                     Burrito Breakfast    91 Breakfast Spring 2025
    ## 3667  4-Mar                   Small French Omelet    73 Breakfast Spring 2025
    ## 3668  4-Mar                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 3669  4-Mar                             Add Bacon    31 Breakfast Spring 2025
    ## 3670  4-Mar                              Two Eggs     9 Breakfast Spring 2025
    ## 3671  4-Mar                        Pancake Single     3 Breakfast Spring 2025
    ## 3672  4-Mar                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 3673  4-Mar                              PC Jelly     3 Breakfast Spring 2025
    ## 3674  4-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 3675  4-Mar           Create Your Pasta Bowl MEAT    86   Italian Spring 2025
    ## 3676  4-Mar            Create Your Pasta Bowl VEG    29   Italian Spring 2025
    ## 3677  4-Mar                   Pizza with Toppings    39   Italian Spring 2025
    ## 3678  4-Mar                          Pizza Cheese    24   Italian Spring 2025
    ## 3679  4-Mar                        Add Extra Meat    18   Italian Spring 2025
    ## 3680  4-Mar                      Burrito Bowl BYO    92   Mexican Spring 2025
    ## 3681  4-Mar                           Single Taco     3   Mexican Spring 2025
    ## 3682  4-Mar                        Side Guacamole     2   Mexican Spring 2025
    ## 3683  4-Mar           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 3684  4-Mar            LTO Spicy Chicken Sandwich    33 Grab N Go Spring 2025
    ## 3685  4-Mar                      LTO Meatball Sub    18 Grab N Go Spring 2025
    ## 3686  4-Mar   Egg Cheese Bacon Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3687  4-Mar Egg Cheese Sausage Breakfast Sandwich    19 Grab N Go Spring 2025
    ## 3688  4-Mar                    Salad by the Pound    54 Salad Bar Spring 2025
    ## 3689  4-Mar                            Soup 12 oz    45      Soup Spring 2025
    ## 3690  4-Mar                             8 oz Soup    37      Soup Spring 2025
    ## 3691  5-Mar            Quesadilla Deluxe Trillium   178     Grill Spring 2025
    ## 3692  5-Mar                     Grilled Hamburger    86     Grill Spring 2025
    ## 3693  5-Mar                 Fried Chicken Tenders    88 Grab N Go Spring 2025
    ## 3694  5-Mar         Burrito Una Mano Trillium BYO    64     Grill Spring 2025
    ## 3695  5-Mar                          French Fries   134     Grill Spring 2025
    ## 3696  5-Mar       Grilled Chicken Breast Sandwich    16     Grill Spring 2025
    ## 3697  5-Mar                          + Beef Patty    24     Grill Spring 2025
    ## 3698  5-Mar                     Quesadilla Cheese    11     Grill Spring 2025
    ## 3699  5-Mar                    Sweet Potato Fries    29     Grill Spring 2025
    ## 3700  5-Mar      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 3701  5-Mar                      Side Potato Tots    21 Grab N Go Spring 2025
    ## 3702  5-Mar                  Seared Salmon Burger     4     Grill Spring 2025
    ## 3703  5-Mar                     Black Bean Burger     3     Grill Spring 2025
    ## 3704  5-Mar                    ADD Chicken Breast     3     Grill Spring 2025
    ## 3705  5-Mar                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 3706  5-Mar                            ADD Cheese     7     Grill Spring 2025
    ## 3707  5-Mar                           Add Egg .99     4     Grill Spring 2025
    ## 3708  5-Mar                     1 Entree + 1 Side   143     Asian Spring 2025
    ## 3709  5-Mar                     1 Entree + 2 Side    66     Asian Spring 2025
    ## 3710  5-Mar                    Bowl Ramen Chicken    71     Asian Spring 2025
    ## 3711  5-Mar                   2 Entrees + 2 Sides    22     Asian Spring 2025
    ## 3712  5-Mar                       Bowl Ramen Tofu    20     Asian Spring 2025
    ## 3713  5-Mar                          1 Wok Entree     8     Asian Spring 2025
    ## 3714  5-Mar               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 3715  5-Mar              Side White or Brown Rice    11     Asian Spring 2025
    ## 3716  5-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3717  5-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3718  5-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 3719  5-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 3720  5-Mar           Create Your Pasta Bowl MEAT   132   Italian Spring 2025
    ## 3721  5-Mar            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 3722  5-Mar                   Pizza with Toppings    24   Italian Spring 2025
    ## 3723  5-Mar                        Add Extra Meat    40   Italian Spring 2025
    ## 3724  5-Mar                          Pizza Cheese    20   Italian Spring 2025
    ## 3725  5-Mar              Side Bread Pasta Station     2   Italian Spring 2025
    ## 3726  5-Mar                     Burrito Breakfast   107 Breakfast Spring 2025
    ## 3727  5-Mar                   Small French Omelet    78 Breakfast Spring 2025
    ## 3728  5-Mar                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 3729  5-Mar                             Add Bacon    24 Breakfast Spring 2025
    ## 3730  5-Mar                              Two Eggs    17 Breakfast Spring 2025
    ## 3731  5-Mar                   Trillium Home Fries     7 Breakfast Spring 2025
    ## 3732  5-Mar                        Pancake Single     3 Breakfast Spring 2025
    ## 3733  5-Mar                        2 Slices Toast     4 Breakfast Spring 2025
    ## 3734  5-Mar                                 Toast     3 Breakfast Spring 2025
    ## 3735  5-Mar                              PC Jelly     4 Breakfast Spring 2025
    ## 3736  5-Mar                      Burrito Bowl BYO   101   Mexican Spring 2025
    ## 3737  5-Mar                           Single Taco     3   Mexican Spring 2025
    ## 3738  5-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 3739  5-Mar            LTO Spicy Chicken Sandwich    33 Grab N Go Spring 2025
    ## 3740  5-Mar Egg Cheese Sausage Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 3741  5-Mar   Egg Cheese Bacon Breakfast Sandwich    16 Grab N Go Spring 2025
    ## 3742  5-Mar                            Soup 12 oz    42      Soup Spring 2025
    ## 3743  5-Mar                             8 oz Soup    45      Soup Spring 2025
    ## 3744  5-Mar                    Salad by the Pound    45 Salad Bar Spring 2025
    ## 3745  5-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 3746  6-Mar            Quesadilla Deluxe Trillium   178     Grill Spring 2025
    ## 3747  6-Mar                     Grilled Hamburger   114     Grill Spring 2025
    ## 3748  6-Mar                 Fried Chicken Tenders   121 Grab N Go Spring 2025
    ## 3749  6-Mar         Burrito Una Mano Trillium BYO    81     Grill Spring 2025
    ## 3750  6-Mar                          French Fries   156     Grill Spring 2025
    ## 3751  6-Mar                     Quesadilla Cheese    18     Grill Spring 2025
    ## 3752  6-Mar                    Sweet Potato Fries    41     Grill Spring 2025
    ## 3753  6-Mar       Grilled Chicken Breast Sandwich    12     Grill Spring 2025
    ## 3754  6-Mar      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 3755  6-Mar                          + Beef Patty    20     Grill Spring 2025
    ## 3756  6-Mar                      Side Potato Tots     9 Grab N Go Spring 2025
    ## 3757  6-Mar                  Seared Salmon Burger     3     Grill Spring 2025
    ## 3758  6-Mar                    ADD Chicken Breast     5     Grill Spring 2025
    ## 3759  6-Mar                     Black Bean Burger     1     Grill Spring 2025
    ## 3760  6-Mar                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 3761  6-Mar                           Add Egg .99     2     Grill Spring 2025
    ## 3762  6-Mar                            ADD Cheese     2     Grill Spring 2025
    ## 3763  6-Mar                     1 Entree + 1 Side   171     Asian Spring 2025
    ## 3764  6-Mar                    Bowl Ramen Chicken    72     Asian Spring 2025
    ## 3765  6-Mar                     1 Entree + 2 Side    52     Asian Spring 2025
    ## 3766  6-Mar                   2 Entrees + 2 Sides    35     Asian Spring 2025
    ## 3767  6-Mar                       Bowl Ramen Tofu    24     Asian Spring 2025
    ## 3768  6-Mar                          1 Wok Entree     8     Asian Spring 2025
    ## 3769  6-Mar               Side Vegetarian Lo Mein    11     Asian Spring 2025
    ## 3770  6-Mar              Side White or Brown Rice    11     Asian Spring 2025
    ## 3771  6-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3772  6-Mar                Side Fried Spring Roll     2     Asian Spring 2025
    ## 3773  6-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3774  6-Mar           Create Your Pasta Bowl MEAT    93   Italian Spring 2025
    ## 3775  6-Mar                   Pizza with Toppings    37   Italian Spring 2025
    ## 3776  6-Mar            Create Your Pasta Bowl VEG    23   Italian Spring 2025
    ## 3777  6-Mar                          Pizza Cheese    20   Italian Spring 2025
    ## 3778  6-Mar                        Add Extra Meat    21   Italian Spring 2025
    ## 3779  6-Mar                     Burrito Breakfast    78 Breakfast Spring 2025
    ## 3780  6-Mar                   Small French Omelet    57 Breakfast Spring 2025
    ## 3781  6-Mar                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 3782  6-Mar                             Add Bacon    32 Breakfast Spring 2025
    ## 3783  6-Mar                              Two Eggs     9 Breakfast Spring 2025
    ## 3784  6-Mar                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 3785  6-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3786  6-Mar                      Burrito Bowl BYO   106   Mexican Spring 2025
    ## 3787  6-Mar                           Single Taco     8   Mexican Spring 2025
    ## 3788  6-Mar                        Side Guacamole     2   Mexican Spring 2025
    ## 3789  6-Mar            LTO Spicy Chicken Sandwich    26 Grab N Go Spring 2025
    ## 3790  6-Mar Egg Cheese Sausage Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 3791  6-Mar   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 3792  6-Mar                      LTO Meatball Sub     9 Grab N Go Spring 2025
    ## 3793  6-Mar                    Salad by the Pound    50 Salad Bar Spring 2025
    ## 3794  6-Mar                Add Extra Protein 3.99     4 Salad Bar Spring 2025
    ## 3795  6-Mar                            Soup 12 oz    46      Soup Spring 2025
    ## 3796  6-Mar                             8 oz Soup    31      Soup Spring 2025
    ## 3797  7-Mar            Quesadilla Deluxe Trillium   129     Grill Spring 2025
    ## 3798  7-Mar                     Grilled Hamburger    75     Grill Spring 2025
    ## 3799  7-Mar         Burrito Una Mano Trillium BYO    47     Grill Spring 2025
    ## 3800  7-Mar                 Fried Chicken Tenders    46 Grab N Go Spring 2025
    ## 3801  7-Mar                          French Fries    98     Grill Spring 2025
    ## 3802  7-Mar                     Quesadilla Cheese    21     Grill Spring 2025
    ## 3803  7-Mar                          + Beef Patty    22     Grill Spring 2025
    ## 3804  7-Mar       Grilled Chicken Breast Sandwich     8     Grill Spring 2025
    ## 3805  7-Mar      Trillium Grill Impossible Burger     5     Grill Spring 2025
    ## 3806  7-Mar                    Sweet Potato Fries    15     Grill Spring 2025
    ## 3807  7-Mar                  Seared Salmon Burger     4     Grill Spring 2025
    ## 3808  7-Mar                      Side Potato Tots     9 Grab N Go Spring 2025
    ## 3809  7-Mar                     Black Bean Burger     1     Grill Spring 2025
    ## 3810  7-Mar                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 3811  7-Mar                           Add Egg .99     3     Grill Spring 2025
    ## 3812  7-Mar                            ADD Cheese     3     Grill Spring 2025
    ## 3813  7-Mar                     1 Entree + 1 Side    94     Asian Spring 2025
    ## 3814  7-Mar                    Bowl Ramen Chicken    57     Asian Spring 2025
    ## 3815  7-Mar                     1 Entree + 2 Side    40     Asian Spring 2025
    ## 3816  7-Mar                       Bowl Ramen Tofu    16     Asian Spring 2025
    ## 3817  7-Mar                   2 Entrees + 2 Sides    10     Asian Spring 2025
    ## 3818  7-Mar                          1 Wok Entree     7     Asian Spring 2025
    ## 3819  7-Mar               Side Vegetarian Lo Mein    11     Asian Spring 2025
    ## 3820  7-Mar           Side Vegetable Spring Rolls     5     Asian Spring 2025
    ## 3821  7-Mar              Side White or Brown Rice     7     Asian Spring 2025
    ## 3822  7-Mar                       Side Vegetables     2     Asian Spring 2025
    ## 3823  7-Mar       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 3824  7-Mar                     Burrito Breakfast    73 Breakfast Spring 2025
    ## 3825  7-Mar                   Small French Omelet    38 Breakfast Spring 2025
    ## 3826  7-Mar                  Grand Slam Breakfast     9 Breakfast Spring 2025
    ## 3827  7-Mar                             Add Bacon    15 Breakfast Spring 2025
    ## 3828  7-Mar                   Trillium Home Fries     5 Breakfast Spring 2025
    ## 3829  7-Mar                              Two Eggs     5 Breakfast Spring 2025
    ## 3830  7-Mar                        Pancake Single     3 Breakfast Spring 2025
    ## 3831  7-Mar                        2 Slices Toast     4 Breakfast Spring 2025
    ## 3832  7-Mar                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 3833  7-Mar           Create Your Pasta Bowl MEAT    55   Italian Spring 2025
    ## 3834  7-Mar            Create Your Pasta Bowl VEG    22   Italian Spring 2025
    ## 3835  7-Mar                   Pizza with Toppings    16   Italian Spring 2025
    ## 3836  7-Mar                          Pizza Cheese    16   Italian Spring 2025
    ## 3837  7-Mar                        Add Extra Meat    22   Italian Spring 2025
    ## 3838  7-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 3839  7-Mar                      Burrito Bowl BYO    50   Mexican Spring 2025
    ## 3840  7-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 3841  7-Mar                             8 oz Soup    39      Soup Spring 2025
    ## 3842  7-Mar                            Soup 12 oz    33      Soup Spring 2025
    ## 3843  7-Mar            LTO Spicy Chicken Sandwich    16 Grab N Go Spring 2025
    ## 3844  7-Mar   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 3845  7-Mar Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 3846  7-Mar                    Salad by the Pound    37 Salad Bar Spring 2025
    ## 3847 10-Mar            Quesadilla Deluxe Trillium   182     Grill Spring 2025
    ## 3848 10-Mar                     Grilled Hamburger    86     Grill Spring 2025
    ## 3849 10-Mar         Burrito Una Mano Trillium BYO    75     Grill Spring 2025
    ## 3850 10-Mar                 Fried Chicken Tenders    69 Grab N Go Spring 2025
    ## 3851 10-Mar                          French Fries    98     Grill Spring 2025
    ## 3852 10-Mar                     Quesadilla Cheese    17     Grill Spring 2025
    ## 3853 10-Mar       Grilled Chicken Breast Sandwich    13     Grill Spring 2025
    ## 3854 10-Mar                    Sweet Potato Fries    31     Grill Spring 2025
    ## 3855 10-Mar                          + Beef Patty    21     Grill Spring 2025
    ## 3856 10-Mar      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 3857 10-Mar                  Seared Salmon Burger     8     Grill Spring 2025
    ## 3858 10-Mar                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 3859 10-Mar                     Black Bean Burger     3     Grill Spring 2025
    ## 3860 10-Mar                    ADD Chicken Breast     4     Grill Spring 2025
    ## 3861 10-Mar                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 3862 10-Mar                            ADD Cheese     2     Grill Spring 2025
    ## 3863 10-Mar                           Add Egg .99     1     Grill Spring 2025
    ## 3864 10-Mar                     1 Entree + 1 Side   192     Asian Spring 2025
    ## 3865 10-Mar                     1 Entree + 2 Side    72     Asian Spring 2025
    ## 3866 10-Mar                    Bowl Ramen Chicken    37     Asian Spring 2025
    ## 3867 10-Mar                   2 Entrees + 2 Sides    18     Asian Spring 2025
    ## 3868 10-Mar                       Bowl Ramen Tofu     6     Asian Spring 2025
    ## 3869 10-Mar               Side Vegetarian Lo Mein    10     Asian Spring 2025
    ## 3870 10-Mar                          1 Wok Entree     3     Asian Spring 2025
    ## 3871 10-Mar              Side White or Brown Rice     7     Asian Spring 2025
    ## 3872 10-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 3873 10-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3874 10-Mar           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 3875 10-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 3876 10-Mar           Create Your Pasta Bowl MEAT   106   Italian Spring 2025
    ## 3877 10-Mar            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 3878 10-Mar                   Pizza with Toppings    25   Italian Spring 2025
    ## 3879 10-Mar                          Pizza Cheese    19   Italian Spring 2025
    ## 3880 10-Mar                        Add Extra Meat    22   Italian Spring 2025
    ## 3881 10-Mar                     Burrito Breakfast    88 Breakfast Spring 2025
    ## 3882 10-Mar                   Small French Omelet    47 Breakfast Spring 2025
    ## 3883 10-Mar                             Add Bacon    30 Breakfast Spring 2025
    ## 3884 10-Mar                  Grand Slam Breakfast     5 Breakfast Spring 2025
    ## 3885 10-Mar                              Two Eggs    16 Breakfast Spring 2025
    ## 3886 10-Mar                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 3887 10-Mar                        Pancake Single     4 Breakfast Spring 2025
    ## 3888 10-Mar                        2 Slices Toast     3 Breakfast Spring 2025
    ## 3889 10-Mar                                 Toast     1 Breakfast Spring 2025
    ## 3890 10-Mar                      Burrito Bowl BYO   106   Mexican Spring 2025
    ## 3891 10-Mar                           Single Taco     2   Mexican Spring 2025
    ## 3892 10-Mar                        Side Guacamole     2   Mexican Spring 2025
    ## 3893 10-Mar            LTO Spicy Chicken Sandwich    30 Grab N Go Spring 2025
    ## 3894 10-Mar Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3895 10-Mar   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 3896 10-Mar                    Salad by the Pound    51 Salad Bar Spring 2025
    ## 3897 10-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 3898 10-Mar                             8 oz Soup    39      Soup Spring 2025
    ## 3899 10-Mar                            Soup 12 oz    32      Soup Spring 2025
    ## 3900 11-Mar            Quesadilla Deluxe Trillium   184     Grill Spring 2025
    ## 3901 11-Mar                     Grilled Hamburger   118     Grill Spring 2025
    ## 3902 11-Mar         Burrito Una Mano Trillium BYO    77     Grill Spring 2025
    ## 3903 11-Mar                 Fried Chicken Tenders    79 Grab N Go Spring 2025
    ## 3904 11-Mar                          French Fries   130     Grill Spring 2025
    ## 3905 11-Mar       Grilled Chicken Breast Sandwich    20     Grill Spring 2025
    ## 3906 11-Mar                     Quesadilla Cheese    15     Grill Spring 2025
    ## 3907 11-Mar                    Sweet Potato Fries    32     Grill Spring 2025
    ## 3908 11-Mar                          + Beef Patty    22     Grill Spring 2025
    ## 3909 11-Mar      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 3910 11-Mar                  Seared Salmon Burger     5     Grill Spring 2025
    ## 3911 11-Mar                    ADD Chicken Breast     8     Grill Spring 2025
    ## 3912 11-Mar                     Black Bean Burger     3     Grill Spring 2025
    ## 3913 11-Mar                      Side Potato Tots     8 Grab N Go Spring 2025
    ## 3914 11-Mar                           Add Egg .99     6     Grill Spring 2025
    ## 3915 11-Mar                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 3916 11-Mar                            ADD Cheese     6     Grill Spring 2025
    ## 3917 11-Mar                     1 Entree + 1 Side   184     Asian Spring 2025
    ## 3918 11-Mar                     1 Entree + 2 Side    70     Asian Spring 2025
    ## 3919 11-Mar                    Bowl Ramen Chicken    51     Asian Spring 2025
    ## 3920 11-Mar                   2 Entrees + 2 Sides    34     Asian Spring 2025
    ## 3921 11-Mar                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 3922 11-Mar                          1 Wok Entree     9     Asian Spring 2025
    ## 3923 11-Mar              Side White or Brown Rice    12     Asian Spring 2025
    ## 3924 11-Mar               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 3925 11-Mar                Side Fried Spring Roll     2     Asian Spring 2025
    ## 3926 11-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3927 11-Mar           Create Your Pasta Bowl MEAT    81   Italian Spring 2025
    ## 3928 11-Mar            Create Your Pasta Bowl VEG    29   Italian Spring 2025
    ## 3929 11-Mar                   Pizza with Toppings    35   Italian Spring 2025
    ## 3930 11-Mar                          Pizza Cheese    15   Italian Spring 2025
    ## 3931 11-Mar                        Add Extra Meat    16   Italian Spring 2025
    ## 3932 11-Mar              Side Bread Pasta Station     3   Italian Spring 2025
    ## 3933 11-Mar                     Burrito Breakfast    84 Breakfast Spring 2025
    ## 3934 11-Mar                   Small French Omelet    57 Breakfast Spring 2025
    ## 3935 11-Mar                  Grand Slam Breakfast    10 Breakfast Spring 2025
    ## 3936 11-Mar                             Add Bacon    37 Breakfast Spring 2025
    ## 3937 11-Mar                              Two Eggs     6 Breakfast Spring 2025
    ## 3938 11-Mar                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 3939 11-Mar                        Pancake Single     1 Breakfast Spring 2025
    ## 3940 11-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 3941 11-Mar                                 Toast     1 Breakfast Spring 2025
    ## 3942 11-Mar                              PC Jelly     1 Breakfast Spring 2025
    ## 3943 11-Mar                      Burrito Bowl BYO   110   Mexican Spring 2025
    ## 3944 11-Mar                           Single Taco     9   Mexican Spring 2025
    ## 3945 11-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 3946 11-Mar                            Side Salsa     2   Mexican Spring 2025
    ## 3947 11-Mar            LTO Spicy Chicken Sandwich    28 Grab N Go Spring 2025
    ## 3948 11-Mar                      LTO Meatball Sub    17 Grab N Go Spring 2025
    ## 3949 11-Mar Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 3950 11-Mar   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 3951 11-Mar                    Salad by the Pound    55 Salad Bar Spring 2025
    ## 3952 11-Mar                            Soup 12 oz    39      Soup Spring 2025
    ## 3953 11-Mar                             8 oz Soup    24      Soup Spring 2025
    ## 3954 12-Mar            Quesadilla Deluxe Trillium   173     Grill Spring 2025
    ## 3955 12-Mar                     Grilled Hamburger    89     Grill Spring 2025
    ## 3956 12-Mar                 Fried Chicken Tenders    96 Grab N Go Spring 2025
    ## 3957 12-Mar         Burrito Una Mano Trillium BYO    64     Grill Spring 2025
    ## 3958 12-Mar                          French Fries   126     Grill Spring 2025
    ## 3959 12-Mar       Grilled Chicken Breast Sandwich    19     Grill Spring 2025
    ## 3960 12-Mar                     Quesadilla Cheese    17     Grill Spring 2025
    ## 3961 12-Mar                    Sweet Potato Fries    38     Grill Spring 2025
    ## 3962 12-Mar      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 3963 12-Mar                          + Beef Patty    17     Grill Spring 2025
    ## 3964 12-Mar                  Seared Salmon Burger     7     Grill Spring 2025
    ## 3965 12-Mar                     Black Bean Burger     6     Grill Spring 2025
    ## 3966 12-Mar                      Side Potato Tots     8 Grab N Go Spring 2025
    ## 3967 12-Mar                    ADD Chicken Breast     5     Grill Spring 2025
    ## 3968 12-Mar                           Add Egg .99    10     Grill Spring 2025
    ## 3969 12-Mar                            ADD Cheese     8     Grill Spring 2025
    ## 3970 12-Mar                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 3971 12-Mar                     1 Entree + 1 Side   180     Asian Spring 2025
    ## 3972 12-Mar                     1 Entree + 2 Side    79     Asian Spring 2025
    ## 3973 12-Mar                    Bowl Ramen Chicken    72     Asian Spring 2025
    ## 3974 12-Mar                   2 Entrees + 2 Sides    24     Asian Spring 2025
    ## 3975 12-Mar                       Bowl Ramen Tofu    17     Asian Spring 2025
    ## 3976 12-Mar               Side Vegetarian Lo Mein     8     Asian Spring 2025
    ## 3977 12-Mar                          1 Wok Entree     5     Asian Spring 2025
    ## 3978 12-Mar              Side White or Brown Rice    10     Asian Spring 2025
    ## 3979 12-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 3980 12-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 3981 12-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 3982 12-Mar           Create Your Pasta Bowl MEAT   107   Italian Spring 2025
    ## 3983 12-Mar                   Pizza with Toppings    36   Italian Spring 2025
    ## 3984 12-Mar            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 3985 12-Mar                        Add Extra Meat    32   Italian Spring 2025
    ## 3986 12-Mar                          Pizza Cheese    16   Italian Spring 2025
    ## 3987 12-Mar                     Burrito Breakfast   108 Breakfast Spring 2025
    ## 3988 12-Mar                   Small French Omelet    61 Breakfast Spring 2025
    ## 3989 12-Mar                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 3990 12-Mar                             Add Bacon    24 Breakfast Spring 2025
    ## 3991 12-Mar                              Two Eggs    15 Breakfast Spring 2025
    ## 3992 12-Mar                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 3993 12-Mar                        Pancake Single     1 Breakfast Spring 2025
    ## 3994 12-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 3995 12-Mar                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 3996 12-Mar                      Burrito Bowl BYO   106   Mexican Spring 2025
    ## 3997 12-Mar                           Single Taco     5   Mexican Spring 2025
    ## 3998 12-Mar                        Side Guacamole     5   Mexican Spring 2025
    ## 3999 12-Mar           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4000 12-Mar                    Salad by the Pound    55 Salad Bar Spring 2025
    ## 4001 12-Mar                            Soup 12 oz    52      Soup Spring 2025
    ## 4002 12-Mar                             8 oz Soup    37      Soup Spring 2025
    ## 4003 12-Mar            LTO Spicy Chicken Sandwich    28 Grab N Go Spring 2025
    ## 4004 12-Mar Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 4005 12-Mar   Egg Cheese Bacon Breakfast Sandwich    19 Grab N Go Spring 2025
    ## 4006 13-Mar            Quesadilla Deluxe Trillium   171     Grill Spring 2025
    ## 4007 13-Mar                     Grilled Hamburger    99     Grill Spring 2025
    ## 4008 13-Mar                 Fried Chicken Tenders   113 Grab N Go Spring 2025
    ## 4009 13-Mar         Burrito Una Mano Trillium BYO    59     Grill Spring 2025
    ## 4010 13-Mar                          French Fries   146     Grill Spring 2025
    ## 4011 13-Mar       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 4012 13-Mar                     Quesadilla Cheese    16     Grill Spring 2025
    ## 4013 13-Mar                    Sweet Potato Fries    40     Grill Spring 2025
    ## 4014 13-Mar                  Seared Salmon Burger     8     Grill Spring 2025
    ## 4015 13-Mar                      Side Potato Tots    18 Grab N Go Spring 2025
    ## 4016 13-Mar      Trillium Grill Impossible Burger     5     Grill Spring 2025
    ## 4017 13-Mar                          + Beef Patty     9     Grill Spring 2025
    ## 4018 13-Mar                    ADD Chicken Breast     4     Grill Spring 2025
    ## 4019 13-Mar                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 4020 13-Mar                            ADD Cheese     5     Grill Spring 2025
    ## 4021 13-Mar                     1 Entree + 1 Side   185     Asian Spring 2025
    ## 4022 13-Mar                    Bowl Ramen Chicken    75     Asian Spring 2025
    ## 4023 13-Mar                     1 Entree + 2 Side    54     Asian Spring 2025
    ## 4024 13-Mar                   2 Entrees + 2 Sides    31     Asian Spring 2025
    ## 4025 13-Mar                       Bowl Ramen Tofu    24     Asian Spring 2025
    ## 4026 13-Mar                          1 Wok Entree    10     Asian Spring 2025
    ## 4027 13-Mar               Side Vegetarian Lo Mein    10     Asian Spring 2025
    ## 4028 13-Mar              Side White or Brown Rice    11     Asian Spring 2025
    ## 4029 13-Mar           Side Vegetable Spring Rolls     4     Asian Spring 2025
    ## 4030 13-Mar                       Side Vegetables     3     Asian Spring 2025
    ## 4031 13-Mar                Side Fried Spring Roll     2     Asian Spring 2025
    ## 4032 13-Mar           Create Your Pasta Bowl MEAT    92   Italian Spring 2025
    ## 4033 13-Mar                   Pizza with Toppings    39   Italian Spring 2025
    ## 4034 13-Mar            Create Your Pasta Bowl VEG    19   Italian Spring 2025
    ## 4035 13-Mar                          Pizza Cheese    18   Italian Spring 2025
    ## 4036 13-Mar                        Add Extra Meat    24   Italian Spring 2025
    ## 4037 13-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4038 13-Mar                     Burrito Breakfast    85 Breakfast Spring 2025
    ## 4039 13-Mar                   Small French Omelet    51 Breakfast Spring 2025
    ## 4040 13-Mar                             Add Bacon    39 Breakfast Spring 2025
    ## 4041 13-Mar                  Grand Slam Breakfast     5 Breakfast Spring 2025
    ## 4042 13-Mar                              Two Eggs    11 Breakfast Spring 2025
    ## 4043 13-Mar                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 4044 13-Mar                        Pancake Single     4 Breakfast Spring 2025
    ## 4045 13-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4046 13-Mar                                 Toast     1 Breakfast Spring 2025
    ## 4047 13-Mar                             PC Butter     1 Breakfast Spring 2025
    ## 4048 13-Mar                      Burrito Bowl BYO    88   Mexican Spring 2025
    ## 4049 13-Mar                           Single Taco     6   Mexican Spring 2025
    ## 4050 13-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 4051 13-Mar                            Side Salsa     3   Mexican Spring 2025
    ## 4052 13-Mar                       Side Sour Cream     3   Mexican Spring 2025
    ## 4053 13-Mar           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4054 13-Mar            LTO Spicy Chicken Sandwich    27 Grab N Go Spring 2025
    ## 4055 13-Mar Egg Cheese Sausage Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 4056 13-Mar   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 4057 13-Mar                      LTO Meatball Sub    10 Grab N Go Spring 2025
    ## 4058 13-Mar                 Burrito Breakfast G&G     5 Grab N Go Spring 2025
    ## 4059 13-Mar                    Salad by the Pound    55 Salad Bar Spring 2025
    ## 4060 13-Mar                Add Extra Protein 3.99     4 Salad Bar Spring 2025
    ## 4061 13-Mar                            Soup 12 oz    49      Soup Spring 2025
    ## 4062 13-Mar                             8 oz Soup    24      Soup Spring 2025
    ## 4063 14-Mar            Quesadilla Deluxe Trillium   111     Grill Spring 2025
    ## 4064 14-Mar                     Grilled Hamburger    76     Grill Spring 2025
    ## 4065 14-Mar         Burrito Una Mano Trillium BYO    51     Grill Spring 2025
    ## 4066 14-Mar                 Fried Chicken Tenders    51 Grab N Go Spring 2025
    ## 4067 14-Mar                          French Fries    73     Grill Spring 2025
    ## 4068 14-Mar                     Quesadilla Cheese    18     Grill Spring 2025
    ## 4069 14-Mar      Trillium Grill Impossible Burger    10     Grill Spring 2025
    ## 4070 14-Mar       Grilled Chicken Breast Sandwich    11     Grill Spring 2025
    ## 4071 14-Mar                    Sweet Potato Fries    30     Grill Spring 2025
    ## 4072 14-Mar                          + Beef Patty    14     Grill Spring 2025
    ## 4073 14-Mar                  Seared Salmon Burger     4     Grill Spring 2025
    ## 4074 14-Mar                      Side Potato Tots    11 Grab N Go Spring 2025
    ## 4075 14-Mar                     Black Bean Burger     1     Grill Spring 2025
    ## 4076 14-Mar                    ADD Chicken Breast     2     Grill Spring 2025
    ## 4077 14-Mar             ADD Burger Salmon Grilled     1     Grill Spring 2025
    ## 4078 14-Mar                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 4079 14-Mar                           Add Egg .99     1     Grill Spring 2025
    ## 4080 14-Mar                     1 Entree + 1 Side   102     Asian Spring 2025
    ## 4081 14-Mar                    Bowl Ramen Chicken    47     Asian Spring 2025
    ## 4082 14-Mar                     1 Entree + 2 Side    36     Asian Spring 2025
    ## 4083 14-Mar                   2 Entrees + 2 Sides    16     Asian Spring 2025
    ## 4084 14-Mar                       Bowl Ramen Tofu    11     Asian Spring 2025
    ## 4085 14-Mar                          1 Wok Entree     3     Asian Spring 2025
    ## 4086 14-Mar               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 4087 14-Mar              Side White or Brown Rice     5     Asian Spring 2025
    ## 4088 14-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 4089 14-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 4090 14-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4091 14-Mar                     Burrito Breakfast    82 Breakfast Spring 2025
    ## 4092 14-Mar                   Small French Omelet    65 Breakfast Spring 2025
    ## 4093 14-Mar                  Grand Slam Breakfast    17 Breakfast Spring 2025
    ## 4094 14-Mar                             Add Bacon    20 Breakfast Spring 2025
    ## 4095 14-Mar                              Two Eggs    11 Breakfast Spring 2025
    ## 4096 14-Mar                        Pancake Single     6 Breakfast Spring 2025
    ## 4097 14-Mar                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 4098 14-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4099 14-Mar                              PC Jelly     3 Breakfast Spring 2025
    ## 4100 14-Mar                      PC Peanut Butter     2 Breakfast Spring 2025
    ## 4101 14-Mar           Create Your Pasta Bowl MEAT    71   Italian Spring 2025
    ## 4102 14-Mar                   Pizza with Toppings    27   Italian Spring 2025
    ## 4103 14-Mar            Create Your Pasta Bowl VEG    14   Italian Spring 2025
    ## 4104 14-Mar                          Pizza Cheese    11   Italian Spring 2025
    ## 4105 14-Mar                        Add Extra Meat    15   Italian Spring 2025
    ## 4106 14-Mar              Side Bread Pasta Station     2   Italian Spring 2025
    ## 4107 14-Mar                      Burrito Bowl BYO    61   Mexican Spring 2025
    ## 4108 14-Mar                           Single Taco     4   Mexican Spring 2025
    ## 4109 14-Mar                        Side Guacamole     1   Mexican Spring 2025
    ## 4110 14-Mar           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 4111 14-Mar            LTO Spicy Chicken Sandwich    33 Grab N Go Spring 2025
    ## 4112 14-Mar Egg Cheese Sausage Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 4113 14-Mar   Egg Cheese Bacon Breakfast Sandwich    19 Grab N Go Spring 2025
    ## 4114 14-Mar                    Salad by the Pound    31 Salad Bar Spring 2025
    ## 4115 14-Mar                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4116 14-Mar                            Soup 12 oz    27      Soup Spring 2025
    ## 4117 14-Mar                             8 oz Soup    15      Soup Spring 2025
    ## 4118 17-Mar            Quesadilla Deluxe Trillium   147     Grill Spring 2025
    ## 4119 17-Mar                     Grilled Hamburger    77     Grill Spring 2025
    ## 4120 17-Mar                 Fried Chicken Tenders    92 Grab N Go Spring 2025
    ## 4121 17-Mar         Burrito Una Mano Trillium BYO    51     Grill Spring 2025
    ## 4122 17-Mar                          French Fries   113     Grill Spring 2025
    ## 4123 17-Mar                     Quesadilla Cheese    19     Grill Spring 2025
    ## 4124 17-Mar       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 4125 17-Mar      Trillium Grill Impossible Burger    12     Grill Spring 2025
    ## 4126 17-Mar                    Sweet Potato Fries    41     Grill Spring 2025
    ## 4127 17-Mar                          + Beef Patty    30     Grill Spring 2025
    ## 4128 17-Mar                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4129 17-Mar                     Black Bean Burger     8     Grill Spring 2025
    ## 4130 17-Mar                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 4131 17-Mar                            ADD Cheese    12     Grill Spring 2025
    ## 4132 17-Mar                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4133 17-Mar                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 4134 17-Mar                           Add Egg .99     2     Grill Spring 2025
    ## 4135 17-Mar                     1 Entree + 1 Side   184     Asian Spring 2025
    ## 4136 17-Mar                    Bowl Ramen Chicken    84     Asian Spring 2025
    ## 4137 17-Mar                     1 Entree + 2 Side    70     Asian Spring 2025
    ## 4138 17-Mar                   2 Entrees + 2 Sides    32     Asian Spring 2025
    ## 4139 17-Mar                       Bowl Ramen Tofu    19     Asian Spring 2025
    ## 4140 17-Mar               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 4141 17-Mar                          1 Wok Entree     3     Asian Spring 2025
    ## 4142 17-Mar              Side White or Brown Rice     7     Asian Spring 2025
    ## 4143 17-Mar                Side Fried Spring Roll     2     Asian Spring 2025
    ## 4144 17-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4145 17-Mar           Create Your Pasta Bowl MEAT   126   Italian Spring 2025
    ## 4146 17-Mar                   Pizza with Toppings    28   Italian Spring 2025
    ## 4147 17-Mar            Create Your Pasta Bowl VEG    18   Italian Spring 2025
    ## 4148 17-Mar                        Add Extra Meat    31   Italian Spring 2025
    ## 4149 17-Mar                          Pizza Cheese    17   Italian Spring 2025
    ## 4150 17-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4151 17-Mar                     Burrito Breakfast    76 Breakfast Spring 2025
    ## 4152 17-Mar                   Small French Omelet    61 Breakfast Spring 2025
    ## 4153 17-Mar                             Add Bacon    32 Breakfast Spring 2025
    ## 4154 17-Mar                  Grand Slam Breakfast     7 Breakfast Spring 2025
    ## 4155 17-Mar                              Two Eggs    14 Breakfast Spring 2025
    ## 4156 17-Mar                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 4157 17-Mar                      PC Peanut Butter     2 Breakfast Spring 2025
    ## 4158 17-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4159 17-Mar                      Burrito Bowl BYO   100   Mexican Spring 2025
    ## 4160 17-Mar                        Side Guacamole     4   Mexican Spring 2025
    ## 4161 17-Mar           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 4162 17-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4163 17-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4164 17-Mar                    Salad by the Pound    51 Salad Bar Spring 2025
    ## 4165 17-Mar                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4166 17-Mar            LTO Spicy Chicken Sandwich    29 Grab N Go Spring 2025
    ## 4167 17-Mar Egg Cheese Sausage Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 4168 17-Mar   Egg Cheese Bacon Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 4169 17-Mar                             8 oz Soup    47      Soup Spring 2025
    ## 4170 17-Mar                            Soup 12 oz    40      Soup Spring 2025
    ## 4171 18-Mar            Quesadilla Deluxe Trillium   186     Grill Spring 2025
    ## 4172 18-Mar                 Fried Chicken Tenders   105 Grab N Go Spring 2025
    ## 4173 18-Mar                     Grilled Hamburger    80     Grill Spring 2025
    ## 4174 18-Mar         Burrito Una Mano Trillium BYO    69     Grill Spring 2025
    ## 4175 18-Mar                          French Fries   112     Grill Spring 2025
    ## 4176 18-Mar                     Quesadilla Cheese    18     Grill Spring 2025
    ## 4177 18-Mar       Grilled Chicken Breast Sandwich    16     Grill Spring 2025
    ## 4178 18-Mar                    Sweet Potato Fries    43     Grill Spring 2025
    ## 4179 18-Mar      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 4180 18-Mar                          + Beef Patty    18     Grill Spring 2025
    ## 4181 18-Mar                      Side Potato Tots    20 Grab N Go Spring 2025
    ## 4182 18-Mar                  Seared Salmon Burger     6     Grill Spring 2025
    ## 4183 18-Mar                     Black Bean Burger     3     Grill Spring 2025
    ## 4184 18-Mar                    ADD Chicken Breast     4     Grill Spring 2025
    ## 4185 18-Mar           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 4186 18-Mar                            ADD Cheese     8     Grill Spring 2025
    ## 4187 18-Mar                           Add Egg .99     4     Grill Spring 2025
    ## 4188 18-Mar                     1 Entree + 1 Side   196     Asian Spring 2025
    ## 4189 18-Mar                    Bowl Ramen Chicken    73     Asian Spring 2025
    ## 4190 18-Mar                     1 Entree + 2 Side    62     Asian Spring 2025
    ## 4191 18-Mar                   2 Entrees + 2 Sides    33     Asian Spring 2025
    ## 4192 18-Mar                       Bowl Ramen Tofu    23     Asian Spring 2025
    ## 4193 18-Mar               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 4194 18-Mar                          1 Wok Entree     4     Asian Spring 2025
    ## 4195 18-Mar              Side White or Brown Rice     8     Asian Spring 2025
    ## 4196 18-Mar           Side Vegetable Spring Rolls     4     Asian Spring 2025
    ## 4197 18-Mar                Side Fried Spring Roll     3     Asian Spring 2025
    ## 4198 18-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 4199 18-Mar                     Burrito Breakfast    85 Breakfast Spring 2025
    ## 4200 18-Mar                   Small French Omelet    69 Breakfast Spring 2025
    ## 4201 18-Mar                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 4202 18-Mar                             Add Bacon    31 Breakfast Spring 2025
    ## 4203 18-Mar                   Trillium Home Fries     7 Breakfast Spring 2025
    ## 4204 18-Mar                              Two Eggs    10 Breakfast Spring 2025
    ## 4205 18-Mar                        Pancake Single     1 Breakfast Spring 2025
    ## 4206 18-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4207 18-Mar           Create Your Pasta Bowl MEAT    89   Italian Spring 2025
    ## 4208 18-Mar            Create Your Pasta Bowl VEG    27   Italian Spring 2025
    ## 4209 18-Mar                   Pizza with Toppings    31   Italian Spring 2025
    ## 4210 18-Mar                          Pizza Cheese    18   Italian Spring 2025
    ## 4211 18-Mar                        Add Extra Meat    24   Italian Spring 2025
    ## 4212 18-Mar              Side Bread Pasta Station     2   Italian Spring 2025
    ## 4213 18-Mar                      Burrito Bowl BYO   101   Mexican Spring 2025
    ## 4214 18-Mar                           Single Taco     7   Mexican Spring 2025
    ## 4215 18-Mar                        Side Guacamole     4   Mexican Spring 2025
    ## 4216 18-Mar           Add Extra Toppings Una Mano     4   Mexican Spring 2025
    ## 4217 18-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4218 18-Mar            LTO Spicy Chicken Sandwich    24 Grab N Go Spring 2025
    ## 4219 18-Mar Egg Cheese Sausage Breakfast Sandwich    29 Grab N Go Spring 2025
    ## 4220 18-Mar                      LTO Meatball Sub    15 Grab N Go Spring 2025
    ## 4221 18-Mar   Egg Cheese Bacon Breakfast Sandwich    17 Grab N Go Spring 2025
    ## 4222 18-Mar                    Salad by the Pound    57 Salad Bar Spring 2025
    ## 4223 18-Mar                            Soup 12 oz    49      Soup Spring 2025
    ## 4224 18-Mar                             8 oz Soup    24      Soup Spring 2025
    ## 4225 19-Mar            Quesadilla Deluxe Trillium   188     Grill Spring 2025
    ## 4226 19-Mar                     Grilled Hamburger    96     Grill Spring 2025
    ## 4227 19-Mar         Burrito Una Mano Trillium BYO    67     Grill Spring 2025
    ## 4228 19-Mar                 Fried Chicken Tenders    83 Grab N Go Spring 2025
    ## 4229 19-Mar                          French Fries   103     Grill Spring 2025
    ## 4230 19-Mar                     Quesadilla Cheese    27     Grill Spring 2025
    ## 4231 19-Mar       Grilled Chicken Breast Sandwich    20     Grill Spring 2025
    ## 4232 19-Mar                    Sweet Potato Fries    42     Grill Spring 2025
    ## 4233 19-Mar                          + Beef Patty    26     Grill Spring 2025
    ## 4234 19-Mar                  Seared Salmon Burger     6     Grill Spring 2025
    ## 4235 19-Mar                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 4236 19-Mar                     Black Bean Burger     5     Grill Spring 2025
    ## 4237 19-Mar      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 4238 19-Mar                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 4239 19-Mar                    ADD Chicken Breast     2     Grill Spring 2025
    ## 4240 19-Mar                            ADD Cheese    13     Grill Spring 2025
    ## 4241 19-Mar                           Add Egg .99     3     Grill Spring 2025
    ## 4242 19-Mar                     1 Entree + 1 Side   198     Asian Spring 2025
    ## 4243 19-Mar                     1 Entree + 2 Side    68     Asian Spring 2025
    ## 4244 19-Mar                    Bowl Ramen Chicken    62     Asian Spring 2025
    ## 4245 19-Mar                   2 Entrees + 2 Sides    23     Asian Spring 2025
    ## 4246 19-Mar                       Bowl Ramen Tofu    15     Asian Spring 2025
    ## 4247 19-Mar                          1 Wok Entree     5     Asian Spring 2025
    ## 4248 19-Mar              Side White or Brown Rice    10     Asian Spring 2025
    ## 4249 19-Mar               Side Vegetarian Lo Mein     5     Asian Spring 2025
    ## 4250 19-Mar                       Side Vegetables     2     Asian Spring 2025
    ## 4251 19-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 4252 19-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4253 19-Mar                    Add Extra Toppings     1     Asian Spring 2025
    ## 4254 19-Mar           Create Your Pasta Bowl MEAT   106   Italian Spring 2025
    ## 4255 19-Mar            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 4256 19-Mar                   Pizza with Toppings    29   Italian Spring 2025
    ## 4257 19-Mar                          Pizza Cheese    20   Italian Spring 2025
    ## 4258 19-Mar                        Add Extra Meat    22   Italian Spring 2025
    ## 4259 19-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4260 19-Mar                     Burrito Breakfast    95 Breakfast Spring 2025
    ## 4261 19-Mar                   Small French Omelet    45 Breakfast Spring 2025
    ## 4262 19-Mar                  Grand Slam Breakfast     9 Breakfast Spring 2025
    ## 4263 19-Mar                             Add Bacon    28 Breakfast Spring 2025
    ## 4264 19-Mar                              Two Eggs    14 Breakfast Spring 2025
    ## 4265 19-Mar                        Pancake Single     4 Breakfast Spring 2025
    ## 4266 19-Mar                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 4267 19-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4268 19-Mar                      Burrito Bowl BYO   102   Mexican Spring 2025
    ## 4269 19-Mar                        Side Guacamole     1   Mexican Spring 2025
    ## 4270 19-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4271 19-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4272 19-Mar                    Salad by the Pound    67 Salad Bar Spring 2025
    ## 4273 19-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4274 19-Mar            LTO Spicy Chicken Sandwich    39 Grab N Go Spring 2025
    ## 4275 19-Mar Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 4276 19-Mar   Egg Cheese Bacon Breakfast Sandwich    13 Grab N Go Spring 2025
    ## 4277 19-Mar                            Soup 12 oz    41      Soup Spring 2025
    ## 4278 19-Mar                             8 oz Soup    44      Soup Spring 2025
    ## 4279 20-Mar            Quesadilla Deluxe Trillium   180     Grill Spring 2025
    ## 4280 20-Mar                     Grilled Hamburger   103     Grill Spring 2025
    ## 4281 20-Mar         Burrito Una Mano Trillium BYO    65     Grill Spring 2025
    ## 4282 20-Mar                 Fried Chicken Tenders    77 Grab N Go Spring 2025
    ## 4283 20-Mar                          French Fries   109     Grill Spring 2025
    ## 4284 20-Mar                     Quesadilla Cheese    24     Grill Spring 2025
    ## 4285 20-Mar       Grilled Chicken Breast Sandwich    18     Grill Spring 2025
    ## 4286 20-Mar      Trillium Grill Impossible Burger    12     Grill Spring 2025
    ## 4287 20-Mar                    Sweet Potato Fries    23     Grill Spring 2025
    ## 4288 20-Mar                      Side Potato Tots    22 Grab N Go Spring 2025
    ## 4289 20-Mar                          + Beef Patty    14     Grill Spring 2025
    ## 4290 20-Mar                  Seared Salmon Burger     6     Grill Spring 2025
    ## 4291 20-Mar                     Black Bean Burger     3     Grill Spring 2025
    ## 4292 20-Mar                   Add Sausage 2 Patty     5     Grill Spring 2025
    ## 4293 20-Mar                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4294 20-Mar                           Add Egg .99     4     Grill Spring 2025
    ## 4295 20-Mar                            ADD Cheese     5     Grill Spring 2025
    ## 4296 20-Mar                     1 Entree + 1 Side   191     Asian Spring 2025
    ## 4297 20-Mar                     1 Entree + 2 Side    72     Asian Spring 2025
    ## 4298 20-Mar                    Bowl Ramen Chicken    59     Asian Spring 2025
    ## 4299 20-Mar                   2 Entrees + 2 Sides    25     Asian Spring 2025
    ## 4300 20-Mar                       Bowl Ramen Tofu    27     Asian Spring 2025
    ## 4301 20-Mar                          1 Wok Entree    15     Asian Spring 2025
    ## 4302 20-Mar               Side Vegetarian Lo Mein     5     Asian Spring 2025
    ## 4303 20-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4304 20-Mar              Side White or Brown Rice     2     Asian Spring 2025
    ## 4305 20-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 4306 20-Mar           Create Your Pasta Bowl MEAT    98   Italian Spring 2025
    ## 4307 20-Mar            Create Your Pasta Bowl VEG    28   Italian Spring 2025
    ## 4308 20-Mar                   Pizza with Toppings    32   Italian Spring 2025
    ## 4309 20-Mar                          Pizza Cheese    16   Italian Spring 2025
    ## 4310 20-Mar                        Add Extra Meat    20   Italian Spring 2025
    ## 4311 20-Mar              Side Bread Pasta Station     4   Italian Spring 2025
    ## 4312 20-Mar                     Burrito Breakfast    90 Breakfast Spring 2025
    ## 4313 20-Mar                   Small French Omelet    50 Breakfast Spring 2025
    ## 4314 20-Mar                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 4315 20-Mar                             Add Bacon    40 Breakfast Spring 2025
    ## 4316 20-Mar                              Two Eggs    17 Breakfast Spring 2025
    ## 4317 20-Mar                   Trillium Home Fries     1 Breakfast Spring 2025
    ## 4318 20-Mar                        Pancake Single     1 Breakfast Spring 2025
    ## 4319 20-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4320 20-Mar                      Burrito Bowl BYO   107   Mexican Spring 2025
    ## 4321 20-Mar                           Single Taco     9   Mexican Spring 2025
    ## 4322 20-Mar                        Side Guacamole     9   Mexican Spring 2025
    ## 4323 20-Mar           Add Extra Toppings Una Mano     4   Mexican Spring 2025
    ## 4324 20-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4325 20-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4326 20-Mar                      LTO Meatball Sub    22 Grab N Go Spring 2025
    ## 4327 20-Mar            LTO Spicy Chicken Sandwich    18 Grab N Go Spring 2025
    ## 4328 20-Mar Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 4329 20-Mar   Egg Cheese Bacon Breakfast Sandwich    18 Grab N Go Spring 2025
    ## 4330 20-Mar                    Salad by the Pound    55 Salad Bar Spring 2025
    ## 4331 20-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4332 20-Mar                            Soup 12 oz    58      Soup Spring 2025
    ## 4333 20-Mar                             8 oz Soup    20      Soup Spring 2025
    ## 4334 21-Mar            Quesadilla Deluxe Trillium   136     Grill Spring 2025
    ## 4335 21-Mar                     Grilled Hamburger    85     Grill Spring 2025
    ## 4336 21-Mar         Burrito Una Mano Trillium BYO    46     Grill Spring 2025
    ## 4337 21-Mar                 Fried Chicken Tenders    55 Grab N Go Spring 2025
    ## 4338 21-Mar                          French Fries    80     Grill Spring 2025
    ## 4339 21-Mar                    Sweet Potato Fries    31     Grill Spring 2025
    ## 4340 21-Mar                     Quesadilla Cheese    10     Grill Spring 2025
    ## 4341 21-Mar       Grilled Chicken Breast Sandwich     9     Grill Spring 2025
    ## 4342 21-Mar                  Seared Salmon Burger     7     Grill Spring 2025
    ## 4343 21-Mar      Trillium Grill Impossible Burger     5     Grill Spring 2025
    ## 4344 21-Mar                      Side Potato Tots    12 Grab N Go Spring 2025
    ## 4345 21-Mar                          + Beef Patty     6     Grill Spring 2025
    ## 4346 21-Mar                     Black Bean Burger     1     Grill Spring 2025
    ## 4347 21-Mar                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4348 21-Mar                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 4349 21-Mar                            ADD Cheese     1     Grill Spring 2025
    ## 4350 21-Mar                     1 Entree + 1 Side   109     Asian Spring 2025
    ## 4351 21-Mar                     1 Entree + 2 Side    49     Asian Spring 2025
    ## 4352 21-Mar                    Bowl Ramen Chicken    40     Asian Spring 2025
    ## 4353 21-Mar                   2 Entrees + 2 Sides    19     Asian Spring 2025
    ## 4354 21-Mar                       Bowl Ramen Tofu    10     Asian Spring 2025
    ## 4355 21-Mar                          1 Wok Entree     2     Asian Spring 2025
    ## 4356 21-Mar               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 4357 21-Mar              Side White or Brown Rice     5     Asian Spring 2025
    ## 4358 21-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 4359 21-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 4360 21-Mar           Create Your Pasta Bowl MEAT    69   Italian Spring 2025
    ## 4361 21-Mar            Create Your Pasta Bowl VEG    18   Italian Spring 2025
    ## 4362 21-Mar                   Pizza with Toppings    22   Italian Spring 2025
    ## 4363 21-Mar                          Pizza Cheese    17   Italian Spring 2025
    ## 4364 21-Mar                        Add Extra Meat    15   Italian Spring 2025
    ## 4365 21-Mar                     Burrito Breakfast    59 Breakfast Spring 2025
    ## 4366 21-Mar                   Small French Omelet    46 Breakfast Spring 2025
    ## 4367 21-Mar                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 4368 21-Mar                             Add Bacon    21 Breakfast Spring 2025
    ## 4369 21-Mar                              Two Eggs     7 Breakfast Spring 2025
    ## 4370 21-Mar                        Pancake Single     4 Breakfast Spring 2025
    ## 4371 21-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4372 21-Mar                                 Toast     1 Breakfast Spring 2025
    ## 4373 21-Mar                              PC Jelly     1 Breakfast Spring 2025
    ## 4374 21-Mar                      Burrito Bowl BYO    59   Mexican Spring 2025
    ## 4375 21-Mar                           Single Taco     2   Mexican Spring 2025
    ## 4376 21-Mar                        Side Guacamole     2   Mexican Spring 2025
    ## 4377 21-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4378 21-Mar            LTO Spicy Chicken Sandwich    23 Grab N Go Spring 2025
    ## 4379 21-Mar Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 4380 21-Mar   Egg Cheese Bacon Breakfast Sandwich    13 Grab N Go Spring 2025
    ## 4381 21-Mar                            Soup 12 oz    37      Soup Spring 2025
    ## 4382 21-Mar                             8 oz Soup    31      Soup Spring 2025
    ## 4383 21-Mar                    Salad by the Pound    31 Salad Bar Spring 2025
    ## 4384 21-Mar                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 4385 24-Mar            Quesadilla Deluxe Trillium   160     Grill Spring 2025
    ## 4386 24-Mar                     Grilled Hamburger    81     Grill Spring 2025
    ## 4387 24-Mar         Burrito Una Mano Trillium BYO    69     Grill Spring 2025
    ## 4388 24-Mar                 Fried Chicken Tenders    66 Grab N Go Spring 2025
    ## 4389 24-Mar                          French Fries    98     Grill Spring 2025
    ## 4390 24-Mar       Grilled Chicken Breast Sandwich    23     Grill Spring 2025
    ## 4391 24-Mar      Trillium Grill Impossible Burger    13     Grill Spring 2025
    ## 4392 24-Mar                     Quesadilla Cheese    16     Grill Spring 2025
    ## 4393 24-Mar                  Seared Salmon Burger    15     Grill Spring 2025
    ## 4394 24-Mar                    Sweet Potato Fries    25     Grill Spring 2025
    ## 4395 24-Mar                          + Beef Patty    10     Grill Spring 2025
    ## 4396 24-Mar                      Side Potato Tots     6 Grab N Go Spring 2025
    ## 4397 24-Mar                     Black Bean Burger     2     Grill Spring 2025
    ## 4398 24-Mar                    ADD Chicken Breast     3     Grill Spring 2025
    ## 4399 24-Mar                   Add Sausage 2 Patty     5     Grill Spring 2025
    ## 4400 24-Mar                           Add Egg .99     2     Grill Spring 2025
    ## 4401 24-Mar                            ADD Cheese     1     Grill Spring 2025
    ## 4402 24-Mar                     1 Entree + 1 Side   196     Asian Spring 2025
    ## 4403 24-Mar                     1 Entree + 2 Side    62     Asian Spring 2025
    ## 4404 24-Mar                    Bowl Ramen Chicken    54     Asian Spring 2025
    ## 4405 24-Mar                   2 Entrees + 2 Sides    25     Asian Spring 2025
    ## 4406 24-Mar                       Bowl Ramen Tofu    13     Asian Spring 2025
    ## 4407 24-Mar                          1 Wok Entree     8     Asian Spring 2025
    ## 4408 24-Mar               Side Vegetarian Lo Mein     9     Asian Spring 2025
    ## 4409 24-Mar              Side White or Brown Rice     8     Asian Spring 2025
    ## 4410 24-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4411 24-Mar           Create Your Pasta Bowl MEAT    87   Italian Spring 2025
    ## 4412 24-Mar                   Pizza with Toppings    36   Italian Spring 2025
    ## 4413 24-Mar            Create Your Pasta Bowl VEG    21   Italian Spring 2025
    ## 4414 24-Mar                          Pizza Cheese    16   Italian Spring 2025
    ## 4415 24-Mar                        Add Extra Meat    24   Italian Spring 2025
    ## 4416 24-Mar                     Burrito Breakfast    93 Breakfast Spring 2025
    ## 4417 24-Mar                   Small French Omelet    51 Breakfast Spring 2025
    ## 4418 24-Mar                  Grand Slam Breakfast    10 Breakfast Spring 2025
    ## 4419 24-Mar                             Add Bacon    19 Breakfast Spring 2025
    ## 4420 24-Mar                              Two Eggs    10 Breakfast Spring 2025
    ## 4421 24-Mar                   Trillium Home Fries     6 Breakfast Spring 2025
    ## 4422 24-Mar                        Pancake Single     3 Breakfast Spring 2025
    ## 4423 24-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4424 24-Mar                              PC Jelly     2 Breakfast Spring 2025
    ## 4425 24-Mar                                 Toast     1 Breakfast Spring 2025
    ## 4426 24-Mar                      Burrito Bowl BYO   104   Mexican Spring 2025
    ## 4427 24-Mar                           Single Taco     2   Mexican Spring 2025
    ## 4428 24-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 4429 24-Mar                       Side Sour Cream     4   Mexican Spring 2025
    ## 4430 24-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4431 24-Mar                    Salad by the Pound    57 Salad Bar Spring 2025
    ## 4432 24-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4433 24-Mar                            Soup 12 oz    44      Soup Spring 2025
    ## 4434 24-Mar                             8 oz Soup    46      Soup Spring 2025
    ## 4435 24-Mar            LTO Spicy Chicken Sandwich    26 Grab N Go Spring 2025
    ## 4436 24-Mar   Egg Cheese Bacon Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 4437 24-Mar Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 4438 25-Mar            Quesadilla Deluxe Trillium   185     Grill Spring 2025
    ## 4439 25-Mar                     Grilled Hamburger   122     Grill Spring 2025
    ## 4440 25-Mar                 Fried Chicken Tenders   107 Grab N Go Spring 2025
    ## 4441 25-Mar         Burrito Una Mano Trillium BYO    68     Grill Spring 2025
    ## 4442 25-Mar                          French Fries   135     Grill Spring 2025
    ## 4443 25-Mar                     Quesadilla Cheese    18     Grill Spring 2025
    ## 4444 25-Mar       Grilled Chicken Breast Sandwich    15     Grill Spring 2025
    ## 4445 25-Mar                    Sweet Potato Fries    31     Grill Spring 2025
    ## 4446 25-Mar                  Seared Salmon Burger     8     Grill Spring 2025
    ## 4447 25-Mar      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 4448 25-Mar                     Black Bean Burger     5     Grill Spring 2025
    ## 4449 25-Mar                          + Beef Patty     9     Grill Spring 2025
    ## 4450 25-Mar                      Side Potato Tots     9 Grab N Go Spring 2025
    ## 4451 25-Mar                    ADD Chicken Breast     2     Grill Spring 2025
    ## 4452 25-Mar                           Add Egg .99     7     Grill Spring 2025
    ## 4453 25-Mar                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 4454 25-Mar                     1 Entree + 1 Side   199     Asian Spring 2025
    ## 4455 25-Mar                    Bowl Ramen Chicken    85     Asian Spring 2025
    ## 4456 25-Mar                     1 Entree + 2 Side    74     Asian Spring 2025
    ## 4457 25-Mar                   2 Entrees + 2 Sides    38     Asian Spring 2025
    ## 4458 25-Mar                       Bowl Ramen Tofu    19     Asian Spring 2025
    ## 4459 25-Mar               Side Vegetarian Lo Mein    10     Asian Spring 2025
    ## 4460 25-Mar              Side White or Brown Rice     8     Asian Spring 2025
    ## 4461 25-Mar                Side Fried Spring Roll     4     Asian Spring 2025
    ## 4462 25-Mar                          1 Wok Entree     2     Asian Spring 2025
    ## 4463 25-Mar           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 4464 25-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 4465 25-Mar                       Side Vegetables     1     Asian Spring 2025
    ## 4466 25-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4467 25-Mar                     Burrito Breakfast   101 Breakfast Spring 2025
    ## 4468 25-Mar                   Small French Omelet    59 Breakfast Spring 2025
    ## 4469 25-Mar                  Grand Slam Breakfast    13 Breakfast Spring 2025
    ## 4470 25-Mar                             Add Bacon    37 Breakfast Spring 2025
    ## 4471 25-Mar                              Two Eggs    14 Breakfast Spring 2025
    ## 4472 25-Mar                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 4473 25-Mar                        Pancake Single     2 Breakfast Spring 2025
    ## 4474 25-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4475 25-Mar                      PC Peanut Butter     2 Breakfast Spring 2025
    ## 4476 25-Mar                              PC Jelly     1 Breakfast Spring 2025
    ## 4477 25-Mar           Create Your Pasta Bowl MEAT    94   Italian Spring 2025
    ## 4478 25-Mar            Create Your Pasta Bowl VEG    24   Italian Spring 2025
    ## 4479 25-Mar                   Pizza with Toppings    25   Italian Spring 2025
    ## 4480 25-Mar                          Pizza Cheese    21   Italian Spring 2025
    ## 4481 25-Mar                        Add Extra Meat    21   Italian Spring 2025
    ## 4482 25-Mar              Side Bread Pasta Station     3   Italian Spring 2025
    ## 4483 25-Mar                      Burrito Bowl BYO   121   Mexican Spring 2025
    ## 4484 25-Mar                           Single Taco    10   Mexican Spring 2025
    ## 4485 25-Mar                        Side Guacamole     3   Mexican Spring 2025
    ## 4486 25-Mar                            Side Salsa     2   Mexican Spring 2025
    ## 4487 25-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4488 25-Mar           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4489 25-Mar            LTO Spicy Chicken Sandwich    27 Grab N Go Spring 2025
    ## 4490 25-Mar                      LTO Meatball Sub    16 Grab N Go Spring 2025
    ## 4491 25-Mar Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 4492 25-Mar   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 4493 25-Mar                    Salad by the Pound    46 Salad Bar Spring 2025
    ## 4494 25-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4495 25-Mar                            Soup 12 oz    47      Soup Spring 2025
    ## 4496 25-Mar                             8 oz Soup    43      Soup Spring 2025
    ## 4497 26-Mar            Quesadilla Deluxe Trillium   181     Grill Spring 2025
    ## 4498 26-Mar                     Grilled Hamburger   108     Grill Spring 2025
    ## 4499 26-Mar                 Fried Chicken Tenders    72 Grab N Go Spring 2025
    ## 4500 26-Mar         Burrito Una Mano Trillium BYO    56     Grill Spring 2025
    ## 4501 26-Mar                          French Fries   119     Grill Spring 2025
    ## 4502 26-Mar       Grilled Chicken Breast Sandwich    19     Grill Spring 2025
    ## 4503 26-Mar                     Quesadilla Cheese    14     Grill Spring 2025
    ## 4504 26-Mar                    Sweet Potato Fries    36     Grill Spring 2025
    ## 4505 26-Mar                     Black Bean Burger     7     Grill Spring 2025
    ## 4506 26-Mar      Trillium Grill Impossible Burger     5     Grill Spring 2025
    ## 4507 26-Mar                  Seared Salmon Burger     6     Grill Spring 2025
    ## 4508 26-Mar                          + Beef Patty    12     Grill Spring 2025
    ## 4509 26-Mar                      Side Potato Tots    10 Grab N Go Spring 2025
    ## 4510 26-Mar                    ADD Chicken Breast     3     Grill Spring 2025
    ## 4511 26-Mar                           Add Egg .99     9     Grill Spring 2025
    ## 4512 26-Mar                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 4513 26-Mar                            ADD Cheese     2     Grill Spring 2025
    ## 4514 26-Mar                     1 Entree + 1 Side   167     Asian Spring 2025
    ## 4515 26-Mar                     1 Entree + 2 Side    74     Asian Spring 2025
    ## 4516 26-Mar                    Bowl Ramen Chicken    52     Asian Spring 2025
    ## 4517 26-Mar                   2 Entrees + 2 Sides    16     Asian Spring 2025
    ## 4518 26-Mar                       Bowl Ramen Tofu    16     Asian Spring 2025
    ## 4519 26-Mar                          1 Wok Entree    14     Asian Spring 2025
    ## 4520 26-Mar               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 4521 26-Mar           Side Vegetable Spring Rolls     6     Asian Spring 2025
    ## 4522 26-Mar              Side White or Brown Rice    10     Asian Spring 2025
    ## 4523 26-Mar                       Side Vegetables     2     Asian Spring 2025
    ## 4524 26-Mar       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4525 26-Mar           Create Your Pasta Bowl MEAT   103   Italian Spring 2025
    ## 4526 26-Mar                   Pizza with Toppings    31   Italian Spring 2025
    ## 4527 26-Mar            Create Your Pasta Bowl VEG    20   Italian Spring 2025
    ## 4528 26-Mar                        Add Extra Meat    32   Italian Spring 2025
    ## 4529 26-Mar                          Pizza Cheese    14   Italian Spring 2025
    ## 4530 26-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4531 26-Mar                     Burrito Breakfast    85 Breakfast Spring 2025
    ## 4532 26-Mar                   Small French Omelet    61 Breakfast Spring 2025
    ## 4533 26-Mar                  Grand Slam Breakfast    11 Breakfast Spring 2025
    ## 4534 26-Mar                             Add Bacon    28 Breakfast Spring 2025
    ## 4535 26-Mar                              Two Eggs    18 Breakfast Spring 2025
    ## 4536 26-Mar                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 4537 26-Mar                        Pancake Single     2 Breakfast Spring 2025
    ## 4538 26-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4539 26-Mar                              PC Jelly     2 Breakfast Spring 2025
    ## 4540 26-Mar                                 Toast     1 Breakfast Spring 2025
    ## 4541 26-Mar                      Burrito Bowl BYO   116   Mexican Spring 2025
    ## 4542 26-Mar                        Side Guacamole     1   Mexican Spring 2025
    ## 4543 26-Mar                            Side Salsa     1   Mexican Spring 2025
    ## 4544 26-Mar                            Soup 12 oz    49      Soup Spring 2025
    ## 4545 26-Mar                             8 oz Soup    41      Soup Spring 2025
    ## 4546 26-Mar            LTO Spicy Chicken Sandwich    26 Grab N Go Spring 2025
    ## 4547 26-Mar   Egg Cheese Bacon Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 4548 26-Mar Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 4549 26-Mar                    Salad by the Pound    39 Salad Bar Spring 2025
    ## 4550 26-Mar                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4551 27-Mar            Quesadilla Deluxe Trillium   166     Grill Spring 2025
    ## 4552 27-Mar                     Grilled Hamburger    99     Grill Spring 2025
    ## 4553 27-Mar                 Fried Chicken Tenders    85 Grab N Go Spring 2025
    ## 4554 27-Mar         Burrito Una Mano Trillium BYO    65     Grill Spring 2025
    ## 4555 27-Mar                          French Fries   130     Grill Spring 2025
    ## 4556 27-Mar                     Quesadilla Cheese    25     Grill Spring 2025
    ## 4557 27-Mar                    Sweet Potato Fries    33     Grill Spring 2025
    ## 4558 27-Mar       Grilled Chicken Breast Sandwich    10     Grill Spring 2025
    ## 4559 27-Mar                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4560 27-Mar      Trillium Grill Impossible Burger     7     Grill Spring 2025
    ## 4561 27-Mar                          + Beef Patty    13     Grill Spring 2025
    ## 4562 27-Mar                      Side Potato Tots    16 Grab N Go Spring 2025
    ## 4563 27-Mar                     Black Bean Burger     1     Grill Spring 2025
    ## 4564 27-Mar                   Add Sausage 2 Patty     3     Grill Spring 2025
    ## 4565 27-Mar                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4566 27-Mar                           Add Egg .99     3     Grill Spring 2025
    ## 4567 27-Mar                            ADD Cheese     1     Grill Spring 2025
    ## 4568 27-Mar                     1 Entree + 1 Side   167     Asian Spring 2025
    ## 4569 27-Mar                     1 Entree + 2 Side    74     Asian Spring 2025
    ## 4570 27-Mar                    Bowl Ramen Chicken    61     Asian Spring 2025
    ## 4571 27-Mar                   2 Entrees + 2 Sides    25     Asian Spring 2025
    ## 4572 27-Mar                       Bowl Ramen Tofu    20     Asian Spring 2025
    ## 4573 27-Mar               Side Vegetarian Lo Mein    10     Asian Spring 2025
    ## 4574 27-Mar                          1 Wok Entree     5     Asian Spring 2025
    ## 4575 27-Mar                Side Fried Spring Roll     4     Asian Spring 2025
    ## 4576 27-Mar              Side White or Brown Rice     5     Asian Spring 2025
    ## 4577 27-Mar                       Side Vegetables     2     Asian Spring 2025
    ## 4578 27-Mar           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4579 27-Mar                     Burrito Breakfast    95 Breakfast Spring 2025
    ## 4580 27-Mar                   Small French Omelet    52 Breakfast Spring 2025
    ## 4581 27-Mar                  Grand Slam Breakfast    17 Breakfast Spring 2025
    ## 4582 27-Mar                             Add Bacon    31 Breakfast Spring 2025
    ## 4583 27-Mar                              Two Eggs    15 Breakfast Spring 2025
    ## 4584 27-Mar                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 4585 27-Mar                        Pancake Single     1 Breakfast Spring 2025
    ## 4586 27-Mar                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4587 27-Mar                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 4588 27-Mar           Create Your Pasta Bowl MEAT    74   Italian Spring 2025
    ## 4589 27-Mar            Create Your Pasta Bowl VEG    27   Italian Spring 2025
    ## 4590 27-Mar                   Pizza with Toppings    32   Italian Spring 2025
    ## 4591 27-Mar                          Pizza Cheese    16   Italian Spring 2025
    ## 4592 27-Mar                        Add Extra Meat    22   Italian Spring 2025
    ## 4593 27-Mar              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4594 27-Mar                      Burrito Bowl BYO    84   Mexican Spring 2025
    ## 4595 27-Mar                           Single Taco     2   Mexican Spring 2025
    ## 4596 27-Mar                        Side Guacamole     4   Mexican Spring 2025
    ## 4597 27-Mar                            Side Salsa     3   Mexican Spring 2025
    ## 4598 27-Mar                       Side Sour Cream     2   Mexican Spring 2025
    ## 4599 27-Mar            LTO Spicy Chicken Sandwich    30 Grab N Go Spring 2025
    ## 4600 27-Mar   Egg Cheese Bacon Breakfast Sandwich    13 Grab N Go Spring 2025
    ## 4601 27-Mar                      LTO Meatball Sub     8 Grab N Go Spring 2025
    ## 4602 27-Mar Egg Cheese Sausage Breakfast Sandwich    10 Grab N Go Spring 2025
    ## 4603 27-Mar                            Soup 12 oz    40      Soup Spring 2025
    ## 4604 27-Mar                             8 oz Soup    29      Soup Spring 2025
    ## 4605 27-Mar                    Salad by the Pound    39 Salad Bar Spring 2025
    ## 4606 27-Mar                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4607 28-Mar            Quesadilla Deluxe Trillium    77     Grill Spring 2025
    ## 4608 28-Mar                     Grilled Hamburger    29     Grill Spring 2025
    ## 4609 28-Mar         Burrito Una Mano Trillium BYO    26     Grill Spring 2025
    ## 4610 28-Mar                 Fried Chicken Tenders    24 Grab N Go Spring 2025
    ## 4611 28-Mar                          French Fries    52     Grill Spring 2025
    ## 4612 28-Mar                     Quesadilla Cheese    13     Grill Spring 2025
    ## 4613 28-Mar       Grilled Chicken Breast Sandwich     8     Grill Spring 2025
    ## 4614 28-Mar                     Black Bean Burger     5     Grill Spring 2025
    ## 4615 28-Mar                  Seared Salmon Burger     5     Grill Spring 2025
    ## 4616 28-Mar                    Sweet Potato Fries    14     Grill Spring 2025
    ## 4617 28-Mar      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 4618 28-Mar                      Side Potato Tots     7 Grab N Go Spring 2025
    ## 4619 28-Mar                          + Beef Patty     4     Grill Spring 2025
    ## 4620 28-Mar                    ADD Chicken Breast     3     Grill Spring 2025
    ## 4621 28-Mar                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 4622 28-Mar                           Add Egg .99     1     Grill Spring 2025
    ## 4623 28-Mar                     1 Entree + 1 Side    62     Asian Spring 2025
    ## 4624 28-Mar                    Bowl Ramen Chicken    32     Asian Spring 2025
    ## 4625 28-Mar                     1 Entree + 2 Side    26     Asian Spring 2025
    ## 4626 28-Mar                   2 Entrees + 2 Sides    13     Asian Spring 2025
    ## 4627 28-Mar                       Bowl Ramen Tofu     6     Asian Spring 2025
    ## 4628 28-Mar                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 4629 28-Mar               Side Vegetarian Lo Mein     2     Asian Spring 2025
    ## 4630 28-Mar                          1 Wok Entree     1     Asian Spring 2025
    ## 4631 28-Mar              Side White or Brown Rice     2     Asian Spring 2025
    ## 4632 28-Mar                Side Fried Spring Roll     1     Asian Spring 2025
    ## 4633 28-Mar                     Burrito Breakfast    65 Breakfast Spring 2025
    ## 4634 28-Mar                   Small French Omelet    37 Breakfast Spring 2025
    ## 4635 28-Mar                  Grand Slam Breakfast     5 Breakfast Spring 2025
    ## 4636 28-Mar                             Add Bacon    11 Breakfast Spring 2025
    ## 4637 28-Mar                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 4638 28-Mar                              Two Eggs     6 Breakfast Spring 2025
    ## 4639 28-Mar                        Pancake Single     4 Breakfast Spring 2025
    ## 4640 28-Mar                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4641 28-Mar           Create Your Pasta Bowl MEAT    34   Italian Spring 2025
    ## 4642 28-Mar                   Pizza with Toppings    19   Italian Spring 2025
    ## 4643 28-Mar            Create Your Pasta Bowl VEG    10   Italian Spring 2025
    ## 4644 28-Mar                          Pizza Cheese    10   Italian Spring 2025
    ## 4645 28-Mar                        Add Extra Meat     7   Italian Spring 2025
    ## 4646 28-Mar              Side Bread Pasta Station     2   Italian Spring 2025
    ## 4647 28-Mar                      Burrito Bowl BYO    36   Mexican Spring 2025
    ## 4648 28-Mar                           Single Taco     5   Mexican Spring 2025
    ## 4649 28-Mar                             8 oz Soup    23      Soup Spring 2025
    ## 4650 28-Mar                            Soup 12 oz    18      Soup Spring 2025
    ## 4651 28-Mar            LTO Spicy Chicken Sandwich    12 Grab N Go Spring 2025
    ## 4652 28-Mar                      LTO Meatball Sub    10 Grab N Go Spring 2025
    ## 4653 28-Mar                    Salad by the Pound    15 Salad Bar Spring 2025
    ## 4654 28-Mar                Add Extra Protein 3.99     4 Salad Bar Spring 2025
    ## 4655  7-Apr            Quesadilla Deluxe Trillium   142     Grill Spring 2025
    ## 4656  7-Apr                     Grilled Hamburger    77     Grill Spring 2025
    ## 4657  7-Apr                 Fried Chicken Tenders    57 Grab N Go Spring 2025
    ## 4658  7-Apr         Burrito Una Mano Trillium BYO    42     Grill Spring 2025
    ## 4659  7-Apr                          French Fries    89     Grill Spring 2025
    ## 4660  7-Apr                     Quesadilla Cheese    19     Grill Spring 2025
    ## 4661  7-Apr       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 4662  7-Apr                    Sweet Potato Fries    27     Grill Spring 2025
    ## 4663  7-Apr                          + Beef Patty    13     Grill Spring 2025
    ## 4664  7-Apr                     Black Bean Burger     4     Grill Spring 2025
    ## 4665  7-Apr                  Seared Salmon Burger     4     Grill Spring 2025
    ## 4666  7-Apr      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 4667  7-Apr                      Side Potato Tots     8 Grab N Go Spring 2025
    ## 4668  7-Apr                    ADD Chicken Breast     2     Grill Spring 2025
    ## 4669  7-Apr                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 4670  7-Apr                           Add Egg .99     2     Grill Spring 2025
    ## 4671  7-Apr                            ADD Cheese     2     Grill Spring 2025
    ## 4672  7-Apr                     1 Entree + 1 Side   152     Asian Spring 2025
    ## 4673  7-Apr                     1 Entree + 2 Side    62     Asian Spring 2025
    ## 4674  7-Apr                    Bowl Ramen Chicken    50     Asian Spring 2025
    ## 4675  7-Apr                   2 Entrees + 2 Sides    17     Asian Spring 2025
    ## 4676  7-Apr                       Bowl Ramen Tofu    17     Asian Spring 2025
    ## 4677  7-Apr                          1 Wok Entree     6     Asian Spring 2025
    ## 4678  7-Apr               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 4679  7-Apr              Side White or Brown Rice     8     Asian Spring 2025
    ## 4680  7-Apr                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 4681  7-Apr           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 4682  7-Apr           Create Your Pasta Bowl MEAT    93   Italian Spring 2025
    ## 4683  7-Apr            Create Your Pasta Bowl VEG    21   Italian Spring 2025
    ## 4684  7-Apr                   Pizza with Toppings    22   Italian Spring 2025
    ## 4685  7-Apr                        Add Extra Meat    33   Italian Spring 2025
    ## 4686  7-Apr                          Pizza Cheese    16   Italian Spring 2025
    ## 4687  7-Apr                     Burrito Breakfast    93 Breakfast Spring 2025
    ## 4688  7-Apr                   Small French Omelet    47 Breakfast Spring 2025
    ## 4689  7-Apr                  Grand Slam Breakfast    12 Breakfast Spring 2025
    ## 4690  7-Apr                             Add Bacon    28 Breakfast Spring 2025
    ## 4691  7-Apr                              Two Eggs    18 Breakfast Spring 2025
    ## 4692  7-Apr                   Trillium Home Fries     7 Breakfast Spring 2025
    ## 4693  7-Apr                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4694  7-Apr                                 Toast     2 Breakfast Spring 2025
    ## 4695  7-Apr                      Burrito Bowl BYO   105   Mexican Spring 2025
    ## 4696  7-Apr                        Side Guacamole     5   Mexican Spring 2025
    ## 4697  7-Apr                           Single Taco     1   Mexican Spring 2025
    ## 4698  7-Apr                            Side Salsa     1   Mexican Spring 2025
    ## 4699  7-Apr                       Side Sour Cream     1   Mexican Spring 2025
    ## 4700  7-Apr                    Salad by the Pound    51 Salad Bar Spring 2025
    ## 4701  7-Apr                Add Extra Protein 3.99     1 Salad Bar Spring 2025
    ## 4702  7-Apr            LTO Spicy Chicken Sandwich    25 Grab N Go Spring 2025
    ## 4703  7-Apr Egg Cheese Sausage Breakfast Sandwich    18 Grab N Go Spring 2025
    ## 4704  7-Apr   Egg Cheese Bacon Breakfast Sandwich    13 Grab N Go Spring 2025
    ## 4705  7-Apr                            Soup 12 oz    31      Soup Spring 2025
    ## 4706  7-Apr                             8 oz Soup    32      Soup Spring 2025
    ## 4707  8-Apr            Quesadilla Deluxe Trillium   176     Grill Spring 2025
    ## 4708  8-Apr                     Grilled Hamburger   113     Grill Spring 2025
    ## 4709  8-Apr                 Fried Chicken Tenders   105 Grab N Go Spring 2025
    ## 4710  8-Apr         Burrito Una Mano Trillium BYO    70     Grill Spring 2025
    ## 4711  8-Apr                          French Fries   124     Grill Spring 2025
    ## 4712  8-Apr       Grilled Chicken Breast Sandwich    28     Grill Spring 2025
    ## 4713  8-Apr                     Quesadilla Cheese    22     Grill Spring 2025
    ## 4714  8-Apr      Trillium Grill Impossible Burger    13     Grill Spring 2025
    ## 4715  8-Apr                    Sweet Potato Fries    40     Grill Spring 2025
    ## 4716  8-Apr                  Seared Salmon Burger    11     Grill Spring 2025
    ## 4717  8-Apr                          + Beef Patty    15     Grill Spring 2025
    ## 4718  8-Apr                      Side Potato Tots    10 Grab N Go Spring 2025
    ## 4719  8-Apr                     Black Bean Burger     2     Grill Spring 2025
    ## 4720  8-Apr                           Add Egg .99     8     Grill Spring 2025
    ## 4721  8-Apr                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 4722  8-Apr                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4723  8-Apr                            ADD Cheese     3     Grill Spring 2025
    ## 4724  8-Apr                     1 Entree + 1 Side   193     Asian Spring 2025
    ## 4725  8-Apr                    Bowl Ramen Chicken    84     Asian Spring 2025
    ## 4726  8-Apr                     1 Entree + 2 Side    62     Asian Spring 2025
    ## 4727  8-Apr                   2 Entrees + 2 Sides    45     Asian Spring 2025
    ## 4728  8-Apr                       Bowl Ramen Tofu    21     Asian Spring 2025
    ## 4729  8-Apr                          1 Wok Entree     8     Asian Spring 2025
    ## 4730  8-Apr               Side Vegetarian Lo Mein     8     Asian Spring 2025
    ## 4731  8-Apr              Side White or Brown Rice     7     Asian Spring 2025
    ## 4732  8-Apr           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 4733  8-Apr       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 4734  8-Apr                       Side Vegetables     1     Asian Spring 2025
    ## 4735  8-Apr                     Burrito Breakfast    96 Breakfast Spring 2025
    ## 4736  8-Apr                   Small French Omelet    77 Breakfast Spring 2025
    ## 4737  8-Apr                  Grand Slam Breakfast    23 Breakfast Spring 2025
    ## 4738  8-Apr                             Add Bacon    34 Breakfast Spring 2025
    ## 4739  8-Apr                              Two Eggs    15 Breakfast Spring 2025
    ## 4740  8-Apr                   Trillium Home Fries     5 Breakfast Spring 2025
    ## 4741  8-Apr                        2 Slices Toast     7 Breakfast Spring 2025
    ## 4742  8-Apr                        Pancake Single     1 Breakfast Spring 2025
    ## 4743  8-Apr                                 Toast     1 Breakfast Spring 2025
    ## 4744  8-Apr                             PC Butter     1 Breakfast Spring 2025
    ## 4745  8-Apr                              PC Jelly     1 Breakfast Spring 2025
    ## 4746  8-Apr                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 4747  8-Apr           Create Your Pasta Bowl MEAT   108   Italian Spring 2025
    ## 4748  8-Apr            Create Your Pasta Bowl VEG    29   Italian Spring 2025
    ## 4749  8-Apr                   Pizza with Toppings    32   Italian Spring 2025
    ## 4750  8-Apr                        Add Extra Meat    29   Italian Spring 2025
    ## 4751  8-Apr                          Pizza Cheese    17   Italian Spring 2025
    ## 4752  8-Apr              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4753  8-Apr                      Burrito Bowl BYO   127   Mexican Spring 2025
    ## 4754  8-Apr                           Single Taco     8   Mexican Spring 2025
    ## 4755  8-Apr                        Side Guacamole     3   Mexican Spring 2025
    ## 4756  8-Apr                            Side Salsa     4   Mexican Spring 2025
    ## 4757  8-Apr           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4758  8-Apr            LTO Spicy Chicken Sandwich    35 Grab N Go Spring 2025
    ## 4759  8-Apr Egg Cheese Sausage Breakfast Sandwich    28 Grab N Go Spring 2025
    ## 4760  8-Apr                      LTO Meatball Sub    14 Grab N Go Spring 2025
    ## 4761  8-Apr   Egg Cheese Bacon Breakfast Sandwich    19 Grab N Go Spring 2025
    ## 4762  8-Apr                    Salad by the Pound    65 Salad Bar Spring 2025
    ## 4763  8-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4764  8-Apr                            Soup 12 oz    65      Soup Spring 2025
    ## 4765  8-Apr                             8 oz Soup    37      Soup Spring 2025
    ## 4766  8-Apr                          LTO Sandwich     3      Deli Spring 2025
    ## 4767  9-Apr            Quesadilla Deluxe Trillium   181     Grill Spring 2025
    ## 4768  9-Apr                     Grilled Hamburger    89     Grill Spring 2025
    ## 4769  9-Apr                 Fried Chicken Tenders    80 Grab N Go Spring 2025
    ## 4770  9-Apr         Burrito Una Mano Trillium BYO    59     Grill Spring 2025
    ## 4771  9-Apr                          French Fries   117     Grill Spring 2025
    ## 4772  9-Apr       Grilled Chicken Breast Sandwich    30     Grill Spring 2025
    ## 4773  9-Apr                     Quesadilla Cheese    20     Grill Spring 2025
    ## 4774  9-Apr                    Sweet Potato Fries    35     Grill Spring 2025
    ## 4775  9-Apr                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4776  9-Apr      Trillium Grill Impossible Burger     6     Grill Spring 2025
    ## 4777  9-Apr                          + Beef Patty    14     Grill Spring 2025
    ## 4778  9-Apr                      Side Potato Tots    11 Grab N Go Spring 2025
    ## 4779  9-Apr                    ADD Chicken Breast     3     Grill Spring 2025
    ## 4780  9-Apr                     Black Bean Burger     1     Grill Spring 2025
    ## 4781  9-Apr                           Add Egg .99     3     Grill Spring 2025
    ## 4782  9-Apr                            ADD Cheese     2     Grill Spring 2025
    ## 4783  9-Apr                     1 Entree + 1 Side   186     Asian Spring 2025
    ## 4784  9-Apr                     1 Entree + 2 Side    84     Asian Spring 2025
    ## 4785  9-Apr                    Bowl Ramen Chicken    69     Asian Spring 2025
    ## 4786  9-Apr                   2 Entrees + 2 Sides    18     Asian Spring 2025
    ## 4787  9-Apr                       Bowl Ramen Tofu    14     Asian Spring 2025
    ## 4788  9-Apr                          1 Wok Entree    17     Asian Spring 2025
    ## 4789  9-Apr               Side Vegetarian Lo Mein     9     Asian Spring 2025
    ## 4790  9-Apr              Side White or Brown Rice     7     Asian Spring 2025
    ## 4791  9-Apr           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4792  9-Apr                Side Fried Spring Roll     1     Asian Spring 2025
    ## 4793  9-Apr                       Side Vegetables     1     Asian Spring 2025
    ## 4794  9-Apr       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4795  9-Apr           Create Your Pasta Bowl MEAT   125   Italian Spring 2025
    ## 4796  9-Apr            Create Your Pasta Bowl VEG    23   Italian Spring 2025
    ## 4797  9-Apr                   Pizza with Toppings    23   Italian Spring 2025
    ## 4798  9-Apr                          Pizza Cheese    16   Italian Spring 2025
    ## 4799  9-Apr                        Add Extra Meat    23   Italian Spring 2025
    ## 4800  9-Apr                     Burrito Breakfast    87 Breakfast Spring 2025
    ## 4801  9-Apr                   Small French Omelet    65 Breakfast Spring 2025
    ## 4802  9-Apr                  Grand Slam Breakfast    14 Breakfast Spring 2025
    ## 4803  9-Apr                             Add Bacon    24 Breakfast Spring 2025
    ## 4804  9-Apr                              Two Eggs    16 Breakfast Spring 2025
    ## 4805  9-Apr                   Trillium Home Fries     6 Breakfast Spring 2025
    ## 4806  9-Apr                        Pancake Single     2 Breakfast Spring 2025
    ## 4807  9-Apr                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 4808  9-Apr                      Burrito Bowl BYO   111   Mexican Spring 2025
    ## 4809  9-Apr                        Side Guacamole     6   Mexican Spring 2025
    ## 4810  9-Apr                            Side Salsa     1   Mexican Spring 2025
    ## 4811  9-Apr                       Side Sour Cream     1   Mexican Spring 2025
    ## 4812  9-Apr                    Salad by the Pound    68 Salad Bar Spring 2025
    ## 4813  9-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4814  9-Apr                            Soup 12 oz    47      Soup Spring 2025
    ## 4815  9-Apr                             8 oz Soup    51      Soup Spring 2025
    ## 4816  9-Apr            LTO Spicy Chicken Sandwich    34 Grab N Go Spring 2025
    ## 4817  9-Apr Egg Cheese Sausage Breakfast Sandwich    28 Grab N Go Spring 2025
    ## 4818  9-Apr   Egg Cheese Bacon Breakfast Sandwich    16 Grab N Go Spring 2025
    ## 4819 10-Apr            Quesadilla Deluxe Trillium   153     Grill Spring 2025
    ## 4820 10-Apr                     Grilled Hamburger    90     Grill Spring 2025
    ## 4821 10-Apr                 Fried Chicken Tenders   104 Grab N Go Spring 2025
    ## 4822 10-Apr         Burrito Una Mano Trillium BYO    77     Grill Spring 2025
    ## 4823 10-Apr                          French Fries   139     Grill Spring 2025
    ## 4824 10-Apr       Grilled Chicken Breast Sandwich    17     Grill Spring 2025
    ## 4825 10-Apr                    Sweet Potato Fries    47     Grill Spring 2025
    ## 4826 10-Apr                     Quesadilla Cheese    15     Grill Spring 2025
    ## 4827 10-Apr                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4828 10-Apr      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 4829 10-Apr                     Black Bean Burger     6     Grill Spring 2025
    ## 4830 10-Apr                          + Beef Patty     6     Grill Spring 2025
    ## 4831 10-Apr                      Side Potato Tots     6 Grab N Go Spring 2025
    ## 4832 10-Apr                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 4833 10-Apr                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4834 10-Apr                            ADD Cheese     4     Grill Spring 2025
    ## 4835 10-Apr                           Add Egg .99     2     Grill Spring 2025
    ## 4836 10-Apr                     1 Entree + 1 Side   204     Asian Spring 2025
    ## 4837 10-Apr                    Bowl Ramen Chicken    79     Asian Spring 2025
    ## 4838 10-Apr                     1 Entree + 2 Side    67     Asian Spring 2025
    ## 4839 10-Apr                   2 Entrees + 2 Sides    29     Asian Spring 2025
    ## 4840 10-Apr                       Bowl Ramen Tofu    24     Asian Spring 2025
    ## 4841 10-Apr               Side Vegetarian Lo Mein    18     Asian Spring 2025
    ## 4842 10-Apr                          1 Wok Entree     9     Asian Spring 2025
    ## 4843 10-Apr           Side Vegetable Spring Rolls     7     Asian Spring 2025
    ## 4844 10-Apr                       Side Vegetables     5     Asian Spring 2025
    ## 4845 10-Apr              Side White or Brown Rice     6     Asian Spring 2025
    ## 4846 10-Apr                Side Fried Spring Roll     3     Asian Spring 2025
    ## 4847 10-Apr                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 4848 10-Apr                     Burrito Breakfast   103 Breakfast Spring 2025
    ## 4849 10-Apr                   Small French Omelet    74 Breakfast Spring 2025
    ## 4850 10-Apr                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 4851 10-Apr                             Add Bacon    33 Breakfast Spring 2025
    ## 4852 10-Apr                              Two Eggs    20 Breakfast Spring 2025
    ## 4853 10-Apr                   Trillium Home Fries     9 Breakfast Spring 2025
    ## 4854 10-Apr                        2 Slices Toast     1 Breakfast Spring 2025
    ## 4855 10-Apr           Create Your Pasta Bowl MEAT    96   Italian Spring 2025
    ## 4856 10-Apr            Create Your Pasta Bowl VEG    29   Italian Spring 2025
    ## 4857 10-Apr                   Pizza with Toppings    29   Italian Spring 2025
    ## 4858 10-Apr                          Pizza Cheese    17   Italian Spring 2025
    ## 4859 10-Apr                        Add Extra Meat    19   Italian Spring 2025
    ## 4860 10-Apr              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4861 10-Apr                      Burrito Bowl BYO   105   Mexican Spring 2025
    ## 4862 10-Apr                           Single Taco     9   Mexican Spring 2025
    ## 4863 10-Apr                        Side Guacamole     5   Mexican Spring 2025
    ## 4864 10-Apr                       Side Sour Cream     6   Mexican Spring 2025
    ## 4865 10-Apr                            Side Salsa     1   Mexican Spring 2025
    ## 4866 10-Apr           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4867 10-Apr            LTO Spicy Chicken Sandwich    23 Grab N Go Spring 2025
    ## 4868 10-Apr   Egg Cheese Bacon Breakfast Sandwich    25 Grab N Go Spring 2025
    ## 4869 10-Apr Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 4870 10-Apr                      LTO Meatball Sub    14 Grab N Go Spring 2025
    ## 4871 10-Apr                            Soup 12 oz    62      Soup Spring 2025
    ## 4872 10-Apr                             8 oz Soup    26      Soup Spring 2025
    ## 4873 10-Apr                    Salad by the Pound    43 Salad Bar Spring 2025
    ## 4874 10-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4875 11-Apr            Quesadilla Deluxe Trillium   129     Grill Spring 2025
    ## 4876 11-Apr                     Grilled Hamburger    72     Grill Spring 2025
    ## 4877 11-Apr         Burrito Una Mano Trillium BYO    51     Grill Spring 2025
    ## 4878 11-Apr                 Fried Chicken Tenders    51 Grab N Go Spring 2025
    ## 4879 11-Apr                          French Fries    85     Grill Spring 2025
    ## 4880 11-Apr                     Quesadilla Cheese    25     Grill Spring 2025
    ## 4881 11-Apr      Trillium Grill Impossible Burger    14     Grill Spring 2025
    ## 4882 11-Apr       Grilled Chicken Breast Sandwich     9     Grill Spring 2025
    ## 4883 11-Apr                  Seared Salmon Burger     7     Grill Spring 2025
    ## 4884 11-Apr                    Sweet Potato Fries    19     Grill Spring 2025
    ## 4885 11-Apr                      Side Potato Tots    15 Grab N Go Spring 2025
    ## 4886 11-Apr                     Black Bean Burger     1     Grill Spring 2025
    ## 4887 11-Apr                          + Beef Patty     2     Grill Spring 2025
    ## 4888 11-Apr                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4889 11-Apr                           Add Egg .99     3     Grill Spring 2025
    ## 4890 11-Apr                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 4891 11-Apr                     1 Entree + 1 Side   109     Asian Spring 2025
    ## 4892 11-Apr                    Bowl Ramen Chicken    46     Asian Spring 2025
    ## 4893 11-Apr                     1 Entree + 2 Side    41     Asian Spring 2025
    ## 4894 11-Apr                   2 Entrees + 2 Sides    15     Asian Spring 2025
    ## 4895 11-Apr                       Bowl Ramen Tofu     8     Asian Spring 2025
    ## 4896 11-Apr                          1 Wok Entree     6     Asian Spring 2025
    ## 4897 11-Apr               Side Vegetarian Lo Mein     9     Asian Spring 2025
    ## 4898 11-Apr                Side Fried Spring Roll     2     Asian Spring 2025
    ## 4899 11-Apr           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4900 11-Apr                       Side Vegetables     1     Asian Spring 2025
    ## 4901 11-Apr                     Burrito Breakfast    89 Breakfast Spring 2025
    ## 4902 11-Apr                   Small French Omelet    66 Breakfast Spring 2025
    ## 4903 11-Apr                  Grand Slam Breakfast     9 Breakfast Spring 2025
    ## 4904 11-Apr                             Add Bacon    23 Breakfast Spring 2025
    ## 4905 11-Apr                        Pancake Single    10 Breakfast Spring 2025
    ## 4906 11-Apr                              Two Eggs    11 Breakfast Spring 2025
    ## 4907 11-Apr                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 4908 11-Apr                                 Toast     1 Breakfast Spring 2025
    ## 4909 11-Apr           Create Your Pasta Bowl MEAT    56   Italian Spring 2025
    ## 4910 11-Apr                   Pizza with Toppings    22   Italian Spring 2025
    ## 4911 11-Apr            Create Your Pasta Bowl VEG    10   Italian Spring 2025
    ## 4912 11-Apr                          Pizza Cheese     9   Italian Spring 2025
    ## 4913 11-Apr                        Add Extra Meat    13   Italian Spring 2025
    ## 4914 11-Apr              Side Bread Pasta Station     1   Italian Spring 2025
    ## 4915 11-Apr                      Burrito Bowl BYO    55   Mexican Spring 2025
    ## 4916 11-Apr                        Side Guacamole     2   Mexican Spring 2025
    ## 4917 11-Apr                            Side Salsa     1   Mexican Spring 2025
    ## 4918 11-Apr            LTO Spicy Chicken Sandwich    22 Grab N Go Spring 2025
    ## 4919 11-Apr   Egg Cheese Bacon Breakfast Sandwich    24 Grab N Go Spring 2025
    ## 4920 11-Apr Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 4921 11-Apr                    Salad by the Pound    32 Salad Bar Spring 2025
    ## 4922 11-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4923 11-Apr                             8 oz Soup    38      Soup Spring 2025
    ## 4924 11-Apr                            Soup 12 oz    21      Soup Spring 2025
    ## 4925 14-Apr            Quesadilla Deluxe Trillium   183     Grill Spring 2025
    ## 4926 14-Apr                     Grilled Hamburger    79     Grill Spring 2025
    ## 4927 14-Apr         Burrito Una Mano Trillium BYO    70     Grill Spring 2025
    ## 4928 14-Apr                 Fried Chicken Tenders    63 Grab N Go Spring 2025
    ## 4929 14-Apr                          French Fries    95     Grill Spring 2025
    ## 4930 14-Apr       Grilled Chicken Breast Sandwich    19     Grill Spring 2025
    ## 4931 14-Apr                     Quesadilla Cheese    19     Grill Spring 2025
    ## 4932 14-Apr      Trillium Grill Impossible Burger    14     Grill Spring 2025
    ## 4933 14-Apr                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4934 14-Apr                    Sweet Potato Fries    24     Grill Spring 2025
    ## 4935 14-Apr                      Side Potato Tots    20 Grab N Go Spring 2025
    ## 4936 14-Apr                          + Beef Patty     7     Grill Spring 2025
    ## 4937 14-Apr                     Black Bean Burger     3     Grill Spring 2025
    ## 4938 14-Apr                    ADD Chicken Breast     3     Grill Spring 2025
    ## 4939 14-Apr             ADD Burger Salmon Grilled     2     Grill Spring 2025
    ## 4940 14-Apr           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 4941 14-Apr                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 4942 14-Apr                           Add Egg .99     4     Grill Spring 2025
    ## 4943 14-Apr                            ADD Cheese     4     Grill Spring 2025
    ## 4944 14-Apr                     1 Entree + 1 Side   197     Asian Spring 2025
    ## 4945 14-Apr                     1 Entree + 2 Side    73     Asian Spring 2025
    ## 4946 14-Apr                    Bowl Ramen Chicken    38     Asian Spring 2025
    ## 4947 14-Apr                   2 Entrees + 2 Sides    23     Asian Spring 2025
    ## 4948 14-Apr                       Bowl Ramen Tofu    27     Asian Spring 2025
    ## 4949 14-Apr                          1 Wok Entree     8     Asian Spring 2025
    ## 4950 14-Apr               Side Vegetarian Lo Mein     6     Asian Spring 2025
    ## 4951 14-Apr              Side White or Brown Rice     6     Asian Spring 2025
    ## 4952 14-Apr                Side Fried Spring Roll     3     Asian Spring 2025
    ## 4953 14-Apr           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 4954 14-Apr       Side Vegetarian Fried Rice with     1     Asian Spring 2025
    ## 4955 14-Apr           Create Your Pasta Bowl MEAT   100   Italian Spring 2025
    ## 4956 14-Apr            Create Your Pasta Bowl VEG    28   Italian Spring 2025
    ## 4957 14-Apr                   Pizza with Toppings    25   Italian Spring 2025
    ## 4958 14-Apr                        Add Extra Meat    33   Italian Spring 2025
    ## 4959 14-Apr                          Pizza Cheese    18   Italian Spring 2025
    ## 4960 14-Apr              Side Bread Pasta Station     2   Italian Spring 2025
    ## 4961 14-Apr                     Burrito Breakfast    73 Breakfast Spring 2025
    ## 4962 14-Apr                   Small French Omelet    55 Breakfast Spring 2025
    ## 4963 14-Apr                  Grand Slam Breakfast     6 Breakfast Spring 2025
    ## 4964 14-Apr                             Add Bacon    20 Breakfast Spring 2025
    ## 4965 14-Apr                              Two Eggs    15 Breakfast Spring 2025
    ## 4966 14-Apr                        Pancake Single     3 Breakfast Spring 2025
    ## 4967 14-Apr                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 4968 14-Apr                        2 Slices Toast     2 Breakfast Spring 2025
    ## 4969 14-Apr                              PC Jelly     2 Breakfast Spring 2025
    ## 4970 14-Apr                      PC Peanut Butter     1 Breakfast Spring 2025
    ## 4971 14-Apr                      Burrito Bowl BYO   103   Mexican Spring 2025
    ## 4972 14-Apr                           Single Taco     7   Mexican Spring 2025
    ## 4973 14-Apr                        Side Guacamole     5   Mexican Spring 2025
    ## 4974 14-Apr                            Side Salsa     2   Mexican Spring 2025
    ## 4975 14-Apr                       Side Sour Cream     2   Mexican Spring 2025
    ## 4976 14-Apr           Add Extra Toppings Una Mano     1   Mexican Spring 2025
    ## 4977 14-Apr                    Salad by the Pound    54 Salad Bar Spring 2025
    ## 4978 14-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 4979 14-Apr            LTO Spicy Chicken Sandwich    25 Grab N Go Spring 2025
    ## 4980 14-Apr   Egg Cheese Bacon Breakfast Sandwich    25 Grab N Go Spring 2025
    ## 4981 14-Apr Egg Cheese Sausage Breakfast Sandwich    20 Grab N Go Spring 2025
    ## 4982 14-Apr                            Soup 12 oz    36      Soup Spring 2025
    ## 4983 14-Apr                             8 oz Soup    32      Soup Spring 2025
    ## 4984 15-Apr            Quesadilla Deluxe Trillium   194     Grill Spring 2025
    ## 4985 15-Apr                     Grilled Hamburger   101     Grill Spring 2025
    ## 4986 15-Apr                 Fried Chicken Tenders   112 Grab N Go Spring 2025
    ## 4987 15-Apr         Burrito Una Mano Trillium BYO    74     Grill Spring 2025
    ## 4988 15-Apr                          French Fries   122     Grill Spring 2025
    ## 4989 15-Apr       Grilled Chicken Breast Sandwich    20     Grill Spring 2025
    ## 4990 15-Apr                    Sweet Potato Fries    47     Grill Spring 2025
    ## 4991 15-Apr                     Quesadilla Cheese    14     Grill Spring 2025
    ## 4992 15-Apr                  Seared Salmon Burger    10     Grill Spring 2025
    ## 4993 15-Apr      Trillium Grill Impossible Burger     8     Grill Spring 2025
    ## 4994 15-Apr                          + Beef Patty    12     Grill Spring 2025
    ## 4995 15-Apr                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 4996 15-Apr                     Black Bean Burger     2     Grill Spring 2025
    ## 4997 15-Apr                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 4998 15-Apr                    ADD Chicken Breast     1     Grill Spring 2025
    ## 4999 15-Apr                           Add Egg .99     1     Grill Spring 2025
    ## 5000 15-Apr                            ADD Cheese     1     Grill Spring 2025
    ## 5001 15-Apr                     1 Entree + 1 Side   220     Asian Spring 2025
    ## 5002 15-Apr                     1 Entree + 2 Side    74     Asian Spring 2025
    ## 5003 15-Apr                    Bowl Ramen Chicken    70     Asian Spring 2025
    ## 5004 15-Apr                   2 Entrees + 2 Sides    29     Asian Spring 2025
    ## 5005 15-Apr                       Bowl Ramen Tofu    28     Asian Spring 2025
    ## 5006 15-Apr                          1 Wok Entree     6     Asian Spring 2025
    ## 5007 15-Apr               Side Vegetarian Lo Mein     7     Asian Spring 2025
    ## 5008 15-Apr              Side White or Brown Rice     5     Asian Spring 2025
    ## 5009 15-Apr                    Bowl Ramen Chicken     1     Asian Spring 2025
    ## 5010 15-Apr                Side Fried Spring Roll     1     Asian Spring 2025
    ## 5011 15-Apr           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 5012 15-Apr                     Burrito Breakfast    90 Breakfast Spring 2025
    ## 5013 15-Apr                   Small French Omelet    53 Breakfast Spring 2025
    ## 5014 15-Apr                  Grand Slam Breakfast    16 Breakfast Spring 2025
    ## 5015 15-Apr                             Add Bacon    27 Breakfast Spring 2025
    ## 5016 15-Apr                              Two Eggs    14 Breakfast Spring 2025
    ## 5017 15-Apr                        Pancake Single     5 Breakfast Spring 2025
    ## 5018 15-Apr                   Trillium Home Fries     2 Breakfast Spring 2025
    ## 5019 15-Apr           Create Your Pasta Bowl MEAT    83   Italian Spring 2025
    ## 5020 15-Apr            Create Your Pasta Bowl VEG    23   Italian Spring 2025
    ## 5021 15-Apr                   Pizza with Toppings    22   Italian Spring 2025
    ## 5022 15-Apr                          Pizza Cheese    25   Italian Spring 2025
    ## 5023 15-Apr                        Add Extra Meat    26   Italian Spring 2025
    ## 5024 15-Apr                      Burrito Bowl BYO   115   Mexican Spring 2025
    ## 5025 15-Apr                        Side Guacamole     4   Mexican Spring 2025
    ## 5026 15-Apr                       Side Sour Cream     7   Mexican Spring 2025
    ## 5027 15-Apr                            Side Salsa     2   Mexican Spring 2025
    ## 5028 15-Apr           Add Extra Toppings Una Mano     2   Mexican Spring 2025
    ## 5029 15-Apr            LTO Spicy Chicken Sandwich    31 Grab N Go Spring 2025
    ## 5030 15-Apr                      LTO Meatball Sub    17 Grab N Go Spring 2025
    ## 5031 15-Apr   Egg Cheese Bacon Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 5032 15-Apr Egg Cheese Sausage Breakfast Sandwich    22 Grab N Go Spring 2025
    ## 5033 15-Apr                    Salad by the Pound    58 Salad Bar Spring 2025
    ## 5034 15-Apr                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 5035 15-Apr                            Soup 12 oz    63      Soup Spring 2025
    ## 5036 15-Apr                             8 oz Soup    26      Soup Spring 2025
    ## 5037 15-Apr                          LTO Sandwich     1      Deli Spring 2025
    ## 5038 16-Apr            Quesadilla Deluxe Trillium   160     Grill Spring 2025
    ## 5039 16-Apr                     Grilled Hamburger   103     Grill Spring 2025
    ## 5040 16-Apr                 Fried Chicken Tenders    79 Grab N Go Spring 2025
    ## 5041 16-Apr         Burrito Una Mano Trillium BYO    49     Grill Spring 2025
    ## 5042 16-Apr                          French Fries   108     Grill Spring 2025
    ## 5043 16-Apr                     Quesadilla Cheese    21     Grill Spring 2025
    ## 5044 16-Apr                    Sweet Potato Fries    37     Grill Spring 2025
    ## 5045 16-Apr       Grilled Chicken Breast Sandwich    13     Grill Spring 2025
    ## 5046 16-Apr      Trillium Grill Impossible Burger     9     Grill Spring 2025
    ## 5047 16-Apr                  Seared Salmon Burger     9     Grill Spring 2025
    ## 5048 16-Apr                      Side Potato Tots    13 Grab N Go Spring 2025
    ## 5049 16-Apr                          + Beef Patty     8     Grill Spring 2025
    ## 5050 16-Apr                     Black Bean Burger     3     Grill Spring 2025
    ## 5051 16-Apr           Add Impossible Burger Patty     1     Grill Spring 2025
    ## 5052 16-Apr                   Add Sausage 2 Patty     2     Grill Spring 2025
    ## 5053 16-Apr                           Add Egg .99     3     Grill Spring 2025
    ## 5054 16-Apr                            ADD Cheese     2     Grill Spring 2025
    ## 5055 16-Apr                     1 Entree + 1 Side   151     Asian Spring 2025
    ## 5056 16-Apr                     1 Entree + 2 Side    63     Asian Spring 2025
    ## 5057 16-Apr                    Bowl Ramen Chicken    61     Asian Spring 2025
    ## 5058 16-Apr                   2 Entrees + 2 Sides    21     Asian Spring 2025
    ## 5059 16-Apr                       Bowl Ramen Tofu    13     Asian Spring 2025
    ## 5060 16-Apr               Side Vegetarian Lo Mein    13     Asian Spring 2025
    ## 5061 16-Apr                Side Fried Spring Roll     4     Asian Spring 2025
    ## 5062 16-Apr                          1 Wok Entree     2     Asian Spring 2025
    ## 5063 16-Apr              Side White or Brown Rice     2     Asian Spring 2025
    ## 5064 16-Apr           Side Vegetable Spring Rolls     1     Asian Spring 2025
    ## 5065 16-Apr           Create Your Pasta Bowl MEAT   118   Italian Spring 2025
    ## 5066 16-Apr            Create Your Pasta Bowl VEG    26   Italian Spring 2025
    ## 5067 16-Apr                   Pizza with Toppings    27   Italian Spring 2025
    ## 5068 16-Apr                        Add Extra Meat    33   Italian Spring 2025
    ## 5069 16-Apr                          Pizza Cheese    16   Italian Spring 2025
    ## 5070 16-Apr                     Burrito Breakfast    98 Breakfast Spring 2025
    ## 5071 16-Apr                   Small French Omelet    63 Breakfast Spring 2025
    ## 5072 16-Apr                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 5073 16-Apr                             Add Bacon    26 Breakfast Spring 2025
    ## 5074 16-Apr                              Two Eggs    21 Breakfast Spring 2025
    ## 5075 16-Apr                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 5076 16-Apr                        Pancake Single     2 Breakfast Spring 2025
    ## 5077 16-Apr                              PC Jelly     1 Breakfast Spring 2025
    ## 5078 16-Apr                      Burrito Bowl BYO   106   Mexican Spring 2025
    ## 5079 16-Apr                           Single Taco     3   Mexican Spring 2025
    ## 5080 16-Apr                        Side Guacamole     1   Mexican Spring 2025
    ## 5081 16-Apr                            Soup 12 oz    73      Soup Spring 2025
    ## 5082 16-Apr                             8 oz Soup    47      Soup Spring 2025
    ## 5083 16-Apr                    Salad by the Pound    49 Salad Bar Spring 2025
    ## 5084 16-Apr            LTO Spicy Chicken Sandwich    22 Grab N Go Spring 2025
    ## 5085 16-Apr   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 5086 16-Apr Egg Cheese Sausage Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 5087 16-Apr                          LTO Sandwich    12      Deli Spring 2025
    ## 5088 17-Apr            Quesadilla Deluxe Trillium   172     Grill Spring 2025
    ## 5089 17-Apr                     Grilled Hamburger   113     Grill Spring 2025
    ## 5090 17-Apr                 Fried Chicken Tenders   121 Grab N Go Spring 2025
    ## 5091 17-Apr         Burrito Una Mano Trillium BYO    72     Grill Spring 2025
    ## 5092 17-Apr                          French Fries   151     Grill Spring 2025
    ## 5093 17-Apr                    Sweet Potato Fries    46     Grill Spring 2025
    ## 5094 17-Apr      Trillium Grill Impossible Burger    13     Grill Spring 2025
    ## 5095 17-Apr       Grilled Chicken Breast Sandwich    16     Grill Spring 2025
    ## 5096 17-Apr                     Quesadilla Cheese    13     Grill Spring 2025
    ## 5097 17-Apr                      Side Potato Tots    21 Grab N Go Spring 2025
    ## 5098 17-Apr                          + Beef Patty    14     Grill Spring 2025
    ## 5099 17-Apr                  Seared Salmon Burger     5     Grill Spring 2025
    ## 5100 17-Apr                     Black Bean Burger     2     Grill Spring 2025
    ## 5101 17-Apr                    ADD Chicken Breast     1     Grill Spring 2025
    ## 5102 17-Apr                   Add Sausage 2 Patty     1     Grill Spring 2025
    ## 5103 17-Apr                           Add Egg .99     2     Grill Spring 2025
    ## 5104 17-Apr                            ADD Cheese     3     Grill Spring 2025
    ## 5105 17-Apr                     1 Entree + 1 Side   188     Asian Spring 2025
    ## 5106 17-Apr                     1 Entree + 2 Side    72     Asian Spring 2025
    ## 5107 17-Apr                    Bowl Ramen Chicken    58     Asian Spring 2025
    ## 5108 17-Apr                   2 Entrees + 2 Sides    29     Asian Spring 2025
    ## 5109 17-Apr                       Bowl Ramen Tofu    24     Asian Spring 2025
    ## 5110 17-Apr                          1 Wok Entree    10     Asian Spring 2025
    ## 5111 17-Apr               Side Vegetarian Lo Mein     4     Asian Spring 2025
    ## 5112 17-Apr              Side White or Brown Rice     7     Asian Spring 2025
    ## 5113 17-Apr           Side Vegetable Spring Rolls     3     Asian Spring 2025
    ## 5114 17-Apr                Side Fried Spring Roll     2     Asian Spring 2025
    ## 5115 17-Apr       Side Vegetarian Fried Rice with     2     Asian Spring 2025
    ## 5116 17-Apr           Create Your Pasta Bowl MEAT    85   Italian Spring 2025
    ## 5117 17-Apr            Create Your Pasta Bowl VEG    27   Italian Spring 2025
    ## 5118 17-Apr                   Pizza with Toppings    30   Italian Spring 2025
    ## 5119 17-Apr                          Pizza Cheese    19   Italian Spring 2025
    ## 5120 17-Apr                        Add Extra Meat    22   Italian Spring 2025
    ## 5121 17-Apr                     Burrito Breakfast    83 Breakfast Spring 2025
    ## 5122 17-Apr                   Small French Omelet    60 Breakfast Spring 2025
    ## 5123 17-Apr                  Grand Slam Breakfast     8 Breakfast Spring 2025
    ## 5124 17-Apr                             Add Bacon    20 Breakfast Spring 2025
    ## 5125 17-Apr                              Two Eggs    19 Breakfast Spring 2025
    ## 5126 17-Apr                   Trillium Home Fries     3 Breakfast Spring 2025
    ## 5127 17-Apr                        Pancake Single     2 Breakfast Spring 2025
    ## 5128 17-Apr                        2 Slices Toast     2 Breakfast Spring 2025
    ## 5129 17-Apr                                 Toast     1 Breakfast Spring 2025
    ## 5130 17-Apr                      Burrito Bowl BYO   126   Mexican Spring 2025
    ## 5131 17-Apr                           Single Taco     2   Mexican Spring 2025
    ## 5132 17-Apr                        Side Guacamole     2   Mexican Spring 2025
    ## 5133 17-Apr                       Side Sour Cream     1   Mexican Spring 2025
    ## 5134 17-Apr            LTO Spicy Chicken Sandwich    28 Grab N Go Spring 2025
    ## 5135 17-Apr                      LTO Meatball Sub    16 Grab N Go Spring 2025
    ## 5136 17-Apr Egg Cheese Sausage Breakfast Sandwich    23 Grab N Go Spring 2025
    ## 5137 17-Apr   Egg Cheese Bacon Breakfast Sandwich    21 Grab N Go Spring 2025
    ## 5138 17-Apr                    Salad by the Pound    54 Salad Bar Spring 2025
    ## 5139 17-Apr                Add Extra Protein 3.99     3 Salad Bar Spring 2025
    ## 5140 17-Apr                            Soup 12 oz    63      Soup Spring 2025
    ## 5141 17-Apr                             8 oz Soup    27      Soup Spring 2025
    ## 5142 18-Apr            Quesadilla Deluxe Trillium   105     Grill Spring 2025
    ## 5143 18-Apr                     Grilled Hamburger    76     Grill Spring 2025
    ## 5144 18-Apr         Burrito Una Mano Trillium BYO    45     Grill Spring 2025
    ## 5145 18-Apr                 Fried Chicken Tenders    54 Grab N Go Spring 2025
    ## 5146 18-Apr                          French Fries    81     Grill Spring 2025
    ## 5147 18-Apr                     Quesadilla Cheese    24     Grill Spring 2025
    ## 5148 18-Apr                  Seared Salmon Burger    10     Grill Spring 2025
    ## 5149 18-Apr                    Sweet Potato Fries    28     Grill Spring 2025
    ## 5150 18-Apr                      Side Potato Tots    20 Grab N Go Spring 2025
    ## 5151 18-Apr       Grilled Chicken Breast Sandwich     6     Grill Spring 2025
    ## 5152 18-Apr      Trillium Grill Impossible Burger     3     Grill Spring 2025
    ## 5153 18-Apr                     Black Bean Burger     2     Grill Spring 2025
    ## 5154 18-Apr                   Add Sausage 2 Patty     4     Grill Spring 2025
    ## 5155 18-Apr                          + Beef Patty     2     Grill Spring 2025
    ## 5156 18-Apr                           Add Egg .99     5     Grill Spring 2025
    ## 5157 18-Apr                            ADD Cheese     2     Grill Spring 2025
    ## 5158 18-Apr                     1 Entree + 1 Side   102     Asian Spring 2025
    ## 5159 18-Apr                     1 Entree + 2 Side    42     Asian Spring 2025
    ## 5160 18-Apr                    Bowl Ramen Chicken    46     Asian Spring 2025
    ## 5161 18-Apr                   2 Entrees + 2 Sides    10     Asian Spring 2025
    ## 5162 18-Apr                       Bowl Ramen Tofu    12     Asian Spring 2025
    ## 5163 18-Apr                          1 Wok Entree     6     Asian Spring 2025
    ## 5164 18-Apr                       Side Vegetables     3     Asian Spring 2025
    ## 5165 18-Apr               Side Vegetarian Lo Mein     3     Asian Spring 2025
    ## 5166 18-Apr           Side Vegetable Spring Rolls     2     Asian Spring 2025
    ## 5167 18-Apr              Side White or Brown Rice     1     Asian Spring 2025
    ## 5168 18-Apr                   Small French Omelet    58 Breakfast Spring 2025
    ## 5169 18-Apr                     Burrito Breakfast    60 Breakfast Spring 2025
    ## 5170 18-Apr                  Grand Slam Breakfast     9 Breakfast Spring 2025
    ## 5171 18-Apr                             Add Bacon    24 Breakfast Spring 2025
    ## 5172 18-Apr                              Two Eggs    11 Breakfast Spring 2025
    ## 5173 18-Apr                        Pancake Single     5 Breakfast Spring 2025
    ## 5174 18-Apr                   Trillium Home Fries     4 Breakfast Spring 2025
    ## 5175 18-Apr                        2 Slices Toast     2 Breakfast Spring 2025
    ## 5176 18-Apr           Create Your Pasta Bowl MEAT    63   Italian Spring 2025
    ## 5177 18-Apr            Create Your Pasta Bowl VEG    18   Italian Spring 2025
    ## 5178 18-Apr                   Pizza with Toppings    19   Italian Spring 2025
    ## 5179 18-Apr                        Add Extra Meat    28   Italian Spring 2025
    ## 5180 18-Apr                          Pizza Cheese    12   Italian Spring 2025
    ## 5181 18-Apr                      Burrito Bowl BYO    66   Mexican Spring 2025
    ## 5182 18-Apr                           Single Taco     3   Mexican Spring 2025
    ## 5183 18-Apr                        Side Guacamole     3   Mexican Spring 2025
    ## 5184 18-Apr                            Side Salsa     1   Mexican Spring 2025
    ## 5185 18-Apr                    Salad by the Pound    39 Salad Bar Spring 2025
    ## 5186 18-Apr                Add Extra Protein 3.99     2 Salad Bar Spring 2025
    ## 5187 18-Apr                             8 oz Soup    34      Soup Spring 2025
    ## 5188 18-Apr                            Soup 12 oz    25      Soup Spring 2025
    ## 5189 18-Apr            LTO Spicy Chicken Sandwich    22 Grab N Go Spring 2025
    ## 5190 18-Apr   Egg Cheese Bacon Breakfast Sandwich    11 Grab N Go Spring 2025
    ## 5191 18-Apr Egg Cheese Sausage Breakfast Sandwich     9 Grab N Go Spring 2025
    ##                  period    station     item_cat station_type meal_period
    ## 1               Control Quesadilla         Main    Satellite   Satellite
    ## 2               Control      Grill         Main    Treatment   Treatment
    ## 3               Control  Grab N Go         Side    Satellite   Satellite
    ## 4               Control Quesadilla         Main    Satellite   Satellite
    ## 5               Control      Grill         Side    Treatment   Treatment
    ## 6               Control Quesadilla         Main    Satellite   Satellite
    ## 7               Control      Grill         Main    Treatment   Treatment
    ## 8               Control      Grill         Main    Treatment   Treatment
    ## 9               Control      Grill         Main    Treatment   Treatment
    ## 10              Control      Grill         Side    Treatment   Treatment
    ## 11              Control      Grill Modification    Treatment   Treatment
    ## 12              Control      Grill         Main    Treatment   Treatment
    ## 13              Control      Grill Modification    Treatment   Treatment
    ## 14              Control      Grill Modification    Treatment   Treatment
    ## 15              Control      Grill Modification    Treatment   Treatment
    ## 16              Control      Grill Modification    Treatment   Treatment
    ## 17              Control      Grill Modification    Treatment   Treatment
    ## 18              Control        Wok         Main    Satellite   Satellite
    ## 19              Control        Wok         Main    Satellite   Satellite
    ## 20              Control      Ramen         Main    Treatment   Treatment
    ## 21              Control        Wok         Main    Satellite   Satellite
    ## 22              Control      Ramen         Main    Treatment   Treatment
    ## 23              Control      Ramen         Main    Treatment   Treatment
    ## 24              Control        Wok         Side    Satellite   Satellite
    ## 25              Control        Wok         Side    Satellite   Satellite
    ## 26              Control        Wok         Side    Satellite   Satellite
    ## 27              Control        Wok         Side    Satellite   Satellite
    ## 28              Control        Wok         Main    Satellite   Satellite
    ## 29              Control        Wok         Side    Satellite   Satellite
    ## 30              Control        Wok         Side    Satellite   Satellite
    ## 31              Control      Pasta         Main    Satellite   Satellite
    ## 32              Control      Pasta         Main    Satellite   Satellite
    ## 33              Control      Pizza         Main    Satellite   Satellite
    ## 34              Control      Pasta Modification    Satellite   Satellite
    ## 35              Control      Pizza         Main    Satellite   Satellite
    ## 36              Control      Pasta         Side    Satellite   Satellite
    ## 37              Control  Breakfast         Main    Satellite   Satellite
    ## 38              Control  Breakfast         Side    Satellite   Satellite
    ## 39              Control  Breakfast         Main    Satellite   Satellite
    ## 40              Control  Breakfast         Main    Satellite   Satellite
    ## 41              Control  Breakfast         Main    Satellite   Satellite
    ## 42              Control  Breakfast Modification    Satellite   Satellite
    ## 43              Control  Breakfast         Side    Satellite   Satellite
    ## 44              Control  Breakfast         Side    Satellite   Satellite
    ## 45              Control  Breakfast         Side    Satellite   Satellite
    ## 46              Control  Breakfast         Side    Satellite   Satellite
    ## 47              Control       Wrap         Main    Satellite   Satellite
    ## 48              Control       Wrap         Main    Satellite   Satellite
    ## 49              Control       Wrap         Side    Satellite   Satellite
    ## 50              Control  Salad Bar         Main    Satellite   Satellite
    ## 51              Control  Salad Bar Modification    Satellite   Satellite
    ## 52              Control       Soup         Main    Satellite   Satellite
    ## 53              Control       Soup         Main    Satellite   Satellite
    ## 54              Control  Grab N Go         Side    Satellite   Satellite
    ## 55              Control Quesadilla         Main    Satellite   Satellite
    ## 56              Control      Grill         Main    Treatment   Treatment
    ## 57              Control  Grab N Go         Side    Satellite   Satellite
    ## 58              Control Quesadilla         Main    Satellite   Satellite
    ## 59              Control      Grill         Side    Treatment   Treatment
    ## 60              Control      Grill         Main    Treatment   Treatment
    ## 61              Control      Grill         Main    Treatment   Treatment
    ## 62              Control Quesadilla         Main    Satellite   Satellite
    ## 63              Control      Grill         Side    Treatment   Treatment
    ## 64              Control      Grill         Main    Treatment   Treatment
    ## 65              Control      Grill Modification    Treatment   Treatment
    ## 66              Control      Grill         Main    Treatment   Treatment
    ## 67              Control      Grill Modification    Treatment   Treatment
    ## 68              Control      Grill Modification    Treatment   Treatment
    ## 69              Control      Grill Modification    Treatment   Treatment
    ## 70              Control      Grill Modification    Treatment   Treatment
    ## 71              Control        Wok         Main    Satellite   Satellite
    ## 72              Control        Wok         Main    Satellite   Satellite
    ## 73              Control      Ramen         Main    Treatment   Treatment
    ## 74              Control        Wok         Main    Satellite   Satellite
    ## 75              Control      Ramen         Main    Treatment   Treatment
    ## 76              Control        Wok         Main    Satellite   Satellite
    ## 77              Control        Wok         Side    Satellite   Satellite
    ## 78              Control        Wok         Side    Satellite   Satellite
    ## 79              Control        Wok         Side    Satellite   Satellite
    ## 80              Control        Wok         Side    Satellite   Satellite
    ## 81              Control      Pasta         Main    Satellite   Satellite
    ## 82              Control      Pasta         Main    Satellite   Satellite
    ## 83              Control      Pizza         Main    Satellite   Satellite
    ## 84              Control      Pizza         Main    Satellite   Satellite
    ## 85              Control      Pasta Modification    Satellite   Satellite
    ## 86              Control      Pasta         Side    Satellite   Satellite
    ## 87              Control  Breakfast         Main    Satellite   Satellite
    ## 88              Control  Breakfast         Side    Satellite   Satellite
    ## 89              Control  Breakfast         Main    Satellite   Satellite
    ## 90              Control  Breakfast         Main    Satellite   Satellite
    ## 91              Control  Breakfast         Main    Satellite   Satellite
    ## 92              Control  Breakfast Modification    Satellite   Satellite
    ## 93              Control  Breakfast         Side    Satellite   Satellite
    ## 94              Control  Breakfast         Side    Satellite   Satellite
    ## 95              Control  Breakfast         Side    Satellite   Satellite
    ## 96              Control  Breakfast         Side    Satellite   Satellite
    ## 97              Control  Breakfast         Side    Satellite   Satellite
    ## 98              Control       Wrap         Main    Satellite   Satellite
    ## 99              Control       Wrap         Main    Satellite   Satellite
    ## 100             Control       Wrap         Side    Satellite   Satellite
    ## 101             Control       Wrap         Side    Satellite   Satellite
    ## 102             Control       Wrap Modification    Satellite   Satellite
    ## 103             Control  Salad Bar         Main    Satellite   Satellite
    ## 104             Control  Salad Bar Modification    Satellite   Satellite
    ## 105             Control       Soup         Main    Satellite   Satellite
    ## 106             Control       Soup         Main    Satellite   Satellite
    ## 107             Control  Grab N Go         Side    Satellite   Satellite
    ## 108             Control Quesadilla         Main    Satellite   Satellite
    ## 109             Control      Grill         Main    Treatment   Treatment
    ## 110             Control  Grab N Go         Side    Satellite   Satellite
    ## 111             Control Quesadilla         Main    Satellite   Satellite
    ## 112             Control      Grill         Side    Treatment   Treatment
    ## 113             Control Quesadilla         Main    Satellite   Satellite
    ## 114             Control      Grill         Main    Treatment   Treatment
    ## 115             Control      Grill         Main    Treatment   Treatment
    ## 116             Control      Grill Modification    Treatment   Treatment
    ## 117             Control      Grill         Main    Treatment   Treatment
    ## 118             Control      Grill         Main    Treatment   Treatment
    ## 119             Control      Grill Modification    Treatment   Treatment
    ## 120             Control      Grill Modification    Treatment   Treatment
    ## 121             Control      Grill Modification    Treatment   Treatment
    ## 122             Control      Grill Modification    Treatment   Treatment
    ## 123             Control        Wok         Main    Satellite   Satellite
    ## 124             Control        Wok         Main    Satellite   Satellite
    ## 125             Control      Ramen         Main    Treatment   Treatment
    ## 126             Control        Wok         Main    Satellite   Satellite
    ## 127             Control      Ramen         Main    Treatment   Treatment
    ## 128             Control        Wok         Side    Satellite   Satellite
    ## 129             Control        Wok         Side    Satellite   Satellite
    ## 130             Control        Wok         Main    Satellite   Satellite
    ## 131             Control        Wok         Side    Satellite   Satellite
    ## 132             Control        Wok         Side    Satellite   Satellite
    ## 133             Control  Breakfast         Main    Satellite   Satellite
    ## 134             Control  Breakfast         Side    Satellite   Satellite
    ## 135             Control  Breakfast         Main    Satellite   Satellite
    ## 136             Control  Breakfast         Main    Satellite   Satellite
    ## 137             Control  Breakfast         Main    Satellite   Satellite
    ## 138             Control  Breakfast Modification    Satellite   Satellite
    ## 139             Control  Breakfast         Side    Satellite   Satellite
    ## 140             Control  Breakfast         Side    Satellite   Satellite
    ## 141             Control  Breakfast         Side    Satellite   Satellite
    ## 142             Control  Breakfast         Side    Satellite   Satellite
    ## 143             Control      Pasta         Main    Satellite   Satellite
    ## 144             Control      Pizza         Main    Satellite   Satellite
    ## 145             Control      Pizza         Main    Satellite   Satellite
    ## 146             Control      Pasta         Main    Satellite   Satellite
    ## 147             Control      Pasta Modification    Satellite   Satellite
    ## 148             Control       Wrap         Main    Satellite   Satellite
    ## 149             Control       Wrap         Main    Satellite   Satellite
    ## 150             Control       Soup         Main    Satellite   Satellite
    ## 151             Control       Soup         Main    Satellite   Satellite
    ## 152             Control  Salad Bar         Main    Satellite   Satellite
    ## 153             Control  Salad Bar Modification    Satellite   Satellite
    ## 154             Control  Grab N Go         Side    Satellite   Satellite
    ## 155             Control Quesadilla         Main    Satellite   Satellite
    ## 156             Control      Grill         Main    Treatment   Treatment
    ## 157             Control Quesadilla         Main    Satellite   Satellite
    ## 158             Control  Grab N Go         Side    Satellite   Satellite
    ## 159             Control      Grill         Side    Treatment   Treatment
    ## 160             Control      Grill         Main    Treatment   Treatment
    ## 161             Control      Grill         Side    Treatment   Treatment
    ## 162             Control      Grill         Main    Treatment   Treatment
    ## 163             Control      Grill         Main    Treatment   Treatment
    ## 164             Control Quesadilla         Main    Satellite   Satellite
    ## 165             Control      Grill Modification    Treatment   Treatment
    ## 166             Control      Grill Modification    Treatment   Treatment
    ## 167             Control      Grill Modification    Treatment   Treatment
    ## 168             Control      Grill         Main    Treatment   Treatment
    ## 169             Control      Grill Modification    Treatment   Treatment
    ## 170             Control      Grill Modification    Treatment   Treatment
    ## 171             Control        Wok         Main    Satellite   Satellite
    ## 172             Control        Wok         Main    Satellite   Satellite
    ## 173             Control      Ramen         Main    Treatment   Treatment
    ## 174             Control        Wok         Main    Satellite   Satellite
    ## 175             Control      Ramen         Main    Treatment   Treatment
    ## 176             Control        Wok         Side    Satellite   Satellite
    ## 177             Control        Wok         Side    Satellite   Satellite
    ## 178             Control        Wok         Main    Satellite   Satellite
    ## 179             Control        Wok         Side    Satellite   Satellite
    ## 180             Control        Wok         Side    Satellite   Satellite
    ## 181             Control      Pasta         Main    Satellite   Satellite
    ## 182             Control      Pasta         Main    Satellite   Satellite
    ## 183             Control      Pizza         Main    Satellite   Satellite
    ## 184             Control      Pizza         Main    Satellite   Satellite
    ## 185             Control      Pasta Modification    Satellite   Satellite
    ## 186             Control      Pasta         Side    Satellite   Satellite
    ## 187             Control  Breakfast         Main    Satellite   Satellite
    ## 188             Control  Breakfast         Side    Satellite   Satellite
    ## 189             Control  Breakfast         Main    Satellite   Satellite
    ## 190             Control  Breakfast         Main    Satellite   Satellite
    ## 191             Control  Breakfast         Main    Satellite   Satellite
    ## 192             Control  Breakfast Modification    Satellite   Satellite
    ## 193             Control  Breakfast         Side    Satellite   Satellite
    ## 194             Control  Breakfast         Side    Satellite   Satellite
    ## 195             Control  Breakfast         Side    Satellite   Satellite
    ## 196             Control  Breakfast         Side    Satellite   Satellite
    ## 197             Control       Wrap         Main    Satellite   Satellite
    ## 198             Control       Wrap         Main    Satellite   Satellite
    ## 199             Control       Wrap         Side    Satellite   Satellite
    ## 200             Control       Wrap Modification    Satellite   Satellite
    ## 201             Control  Salad Bar         Main    Satellite   Satellite
    ## 202             Control       Soup         Main    Satellite   Satellite
    ## 203             Control       Soup         Main    Satellite   Satellite
    ## 204             Control  Grab N Go         Side    Satellite   Satellite
    ## 205             Control Quesadilla         Main    Satellite   Satellite
    ## 206             Control      Grill         Main    Treatment   Treatment
    ## 207             Control  Grab N Go         Side    Satellite   Satellite
    ## 208             Control Quesadilla         Main    Satellite   Satellite
    ## 209             Control      Grill         Side    Treatment   Treatment
    ## 210             Control      Grill         Main    Treatment   Treatment
    ## 211             Control      Grill         Side    Treatment   Treatment
    ## 212             Control Quesadilla         Main    Satellite   Satellite
    ## 213             Control      Grill         Main    Treatment   Treatment
    ## 214             Control      Grill         Main    Treatment   Treatment
    ## 215             Control      Grill Modification    Treatment   Treatment
    ## 216             Control      Grill         Main    Treatment   Treatment
    ## 217             Control      Grill Modification    Treatment   Treatment
    ## 218             Control      Grill Modification    Treatment   Treatment
    ## 219             Control      Grill Modification    Treatment   Treatment
    ## 220             Control      Grill Modification    Treatment   Treatment
    ## 221             Control        Wok         Main    Satellite   Satellite
    ## 222             Control        Wok         Main    Satellite   Satellite
    ## 223             Control      Ramen         Main    Treatment   Treatment
    ## 224             Control        Wok         Main    Satellite   Satellite
    ## 225             Control      Ramen         Main    Treatment   Treatment
    ## 226             Control        Wok         Side    Satellite   Satellite
    ## 227             Control        Wok         Main    Satellite   Satellite
    ## 228             Control        Wok         Side    Satellite   Satellite
    ## 229             Control        Wok         Side    Satellite   Satellite
    ## 230             Control      Pasta         Main    Satellite   Satellite
    ## 231             Control      Pasta         Main    Satellite   Satellite
    ## 232             Control      Pizza         Main    Satellite   Satellite
    ## 233             Control      Pizza         Main    Satellite   Satellite
    ## 234             Control      Pasta Modification    Satellite   Satellite
    ## 235             Control      Pasta         Side    Satellite   Satellite
    ## 236             Control  Breakfast         Main    Satellite   Satellite
    ## 237             Control  Breakfast         Side    Satellite   Satellite
    ## 238             Control  Breakfast         Main    Satellite   Satellite
    ## 239             Control  Breakfast         Main    Satellite   Satellite
    ## 240             Control  Breakfast         Main    Satellite   Satellite
    ## 241             Control  Breakfast Modification    Satellite   Satellite
    ## 242             Control  Breakfast         Side    Satellite   Satellite
    ## 243             Control  Breakfast         Side    Satellite   Satellite
    ## 244             Control  Breakfast         Side    Satellite   Satellite
    ## 245             Control  Breakfast         Side    Satellite   Satellite
    ## 246             Control  Breakfast         Side    Satellite   Satellite
    ## 247             Control  Breakfast         Side    Satellite   Satellite
    ## 248             Control       Wrap         Main    Satellite   Satellite
    ## 249             Control       Wrap         Main    Satellite   Satellite
    ## 250             Control       Wrap         Side    Satellite   Satellite
    ## 251             Control       Wrap Modification    Satellite   Satellite
    ## 252             Control  Salad Bar         Main    Satellite   Satellite
    ## 253             Control       Soup         Main    Satellite   Satellite
    ## 254             Control       Soup         Main    Satellite   Satellite
    ## 255             Control  Grab N Go         Side    Satellite   Satellite
    ## 256             Control Quesadilla         Main    Satellite   Satellite
    ## 257             Control      Grill         Main    Treatment   Treatment
    ## 258             Control  Grab N Go         Side    Satellite   Satellite
    ## 259             Control Quesadilla         Main    Satellite   Satellite
    ## 260             Control      Grill         Side    Treatment   Treatment
    ## 261             Control      Grill         Main    Treatment   Treatment
    ## 262             Control      Grill         Main    Treatment   Treatment
    ## 263             Control Quesadilla         Main    Satellite   Satellite
    ## 264             Control      Grill         Side    Treatment   Treatment
    ## 265             Control      Grill Modification    Treatment   Treatment
    ## 266             Control      Grill         Main    Treatment   Treatment
    ## 267             Control      Grill Modification    Treatment   Treatment
    ## 268             Control      Grill         Main    Treatment   Treatment
    ## 269             Control      Grill Modification    Treatment   Treatment
    ## 270             Control      Grill Modification    Treatment   Treatment
    ## 271             Control      Grill Modification    Treatment   Treatment
    ## 272             Control        Wok         Main    Satellite   Satellite
    ## 273             Control        Wok         Main    Satellite   Satellite
    ## 274             Control      Ramen         Main    Treatment   Treatment
    ## 275             Control        Wok         Main    Satellite   Satellite
    ## 276             Control      Ramen         Main    Treatment   Treatment
    ## 277             Control        Wok         Main    Satellite   Satellite
    ## 278             Control        Wok         Side    Satellite   Satellite
    ## 279             Control        Wok         Side    Satellite   Satellite
    ## 280             Control        Wok         Side    Satellite   Satellite
    ## 281             Control        Wok         Side    Satellite   Satellite
    ## 282             Control        Wok         Side    Satellite   Satellite
    ## 283             Control      Pasta         Main    Satellite   Satellite
    ## 284             Control      Pasta         Main    Satellite   Satellite
    ## 285             Control      Pizza         Main    Satellite   Satellite
    ## 286             Control      Pizza         Main    Satellite   Satellite
    ## 287             Control      Pasta Modification    Satellite   Satellite
    ## 288             Control      Pasta         Side    Satellite   Satellite
    ## 289             Control  Breakfast         Main    Satellite   Satellite
    ## 290             Control  Breakfast         Side    Satellite   Satellite
    ## 291             Control  Breakfast         Main    Satellite   Satellite
    ## 292             Control  Breakfast         Main    Satellite   Satellite
    ## 293             Control  Breakfast         Main    Satellite   Satellite
    ## 294             Control  Breakfast Modification    Satellite   Satellite
    ## 295             Control  Breakfast         Side    Satellite   Satellite
    ## 296             Control  Breakfast         Side    Satellite   Satellite
    ## 297             Control  Breakfast         Side    Satellite   Satellite
    ## 298             Control  Breakfast         Side    Satellite   Satellite
    ## 299             Control  Breakfast         Side    Satellite   Satellite
    ## 300             Control       Wrap         Main    Satellite   Satellite
    ## 301             Control       Wrap         Main    Satellite   Satellite
    ## 302             Control       Wrap         Side    Satellite   Satellite
    ## 303             Control       Wrap Modification    Satellite   Satellite
    ## 304             Control  Salad Bar         Main    Satellite   Satellite
    ## 305             Control  Salad Bar Modification    Satellite   Satellite
    ## 306             Control       Soup         Main    Satellite   Satellite
    ## 307             Control       Soup         Main    Satellite   Satellite
    ## 308             Control  Grab N Go         Side    Satellite   Satellite
    ## 309             Control Quesadilla         Main    Satellite   Satellite
    ## 310             Control      Grill         Main    Treatment   Treatment
    ## 311             Control  Grab N Go         Side    Satellite   Satellite
    ## 312             Control Quesadilla         Main    Satellite   Satellite
    ## 313             Control      Grill         Side    Treatment   Treatment
    ## 314             Control      Grill         Main    Treatment   Treatment
    ## 315             Control      Grill         Main    Treatment   Treatment
    ## 316             Control      Grill         Side    Treatment   Treatment
    ## 317             Control      Grill         Main    Treatment   Treatment
    ## 318             Control Quesadilla         Main    Satellite   Satellite
    ## 319             Control      Grill         Main    Treatment   Treatment
    ## 320             Control      Grill Modification    Treatment   Treatment
    ## 321             Control      Grill Modification    Treatment   Treatment
    ## 322             Control      Grill Modification    Treatment   Treatment
    ## 323             Control      Grill Modification    Treatment   Treatment
    ## 324             Control      Grill Modification    Treatment   Treatment
    ## 325             Control        Wok         Main    Satellite   Satellite
    ## 326             Control        Wok         Main    Satellite   Satellite
    ## 327             Control      Ramen         Main    Treatment   Treatment
    ## 328             Control        Wok         Main    Satellite   Satellite
    ## 329             Control      Ramen         Main    Treatment   Treatment
    ## 330             Control        Wok         Side    Satellite   Satellite
    ## 331             Control        Wok         Main    Satellite   Satellite
    ## 332             Control        Wok         Side    Satellite   Satellite
    ## 333             Control        Wok         Side    Satellite   Satellite
    ## 334             Control      Pasta         Main    Satellite   Satellite
    ## 335             Control      Pasta         Main    Satellite   Satellite
    ## 336             Control      Pizza         Main    Satellite   Satellite
    ## 337             Control      Pizza         Main    Satellite   Satellite
    ## 338             Control      Pasta Modification    Satellite   Satellite
    ## 339             Control      Pasta         Side    Satellite   Satellite
    ## 340             Control  Breakfast         Main    Satellite   Satellite
    ## 341             Control  Breakfast         Side    Satellite   Satellite
    ## 342             Control  Breakfast         Main    Satellite   Satellite
    ## 343             Control  Breakfast         Main    Satellite   Satellite
    ## 344             Control  Breakfast         Main    Satellite   Satellite
    ## 345             Control  Breakfast Modification    Satellite   Satellite
    ## 346             Control  Breakfast         Side    Satellite   Satellite
    ## 347             Control  Breakfast         Side    Satellite   Satellite
    ## 348             Control  Breakfast         Side    Satellite   Satellite
    ## 349             Control  Breakfast         Side    Satellite   Satellite
    ## 350             Control  Breakfast         Side    Satellite   Satellite
    ## 351             Control  Breakfast         Side    Satellite   Satellite
    ## 352             Control  Breakfast         Side    Satellite   Satellite
    ## 353             Control  Salad Bar         Main    Satellite   Satellite
    ## 354             Control  Salad Bar Modification    Satellite   Satellite
    ## 355             Control       Wrap         Main    Satellite   Satellite
    ## 356             Control       Wrap         Side    Satellite   Satellite
    ## 357             Control       Wrap         Main    Satellite   Satellite
    ## 358             Control       Wrap Modification    Satellite   Satellite
    ## 359             Control       Wrap         Side    Satellite   Satellite
    ## 360             Control       Soup         Main    Satellite   Satellite
    ## 361             Control       Soup         Main    Satellite   Satellite
    ## 362             Control  Grab N Go         Side    Satellite   Satellite
    ## 363             Control Quesadilla         Main    Satellite   Satellite
    ## 364             Control      Grill         Main    Treatment   Treatment
    ## 365             Control Quesadilla         Main    Satellite   Satellite
    ## 366             Control  Grab N Go         Side    Satellite   Satellite
    ## 367             Control      Grill         Side    Treatment   Treatment
    ## 368             Control      Grill         Main    Treatment   Treatment
    ## 369             Control      Grill         Main    Treatment   Treatment
    ## 370             Control Quesadilla         Main    Satellite   Satellite
    ## 371             Control      Grill         Main    Treatment   Treatment
    ## 372             Control      Grill         Main    Treatment   Treatment
    ## 373             Control      Grill Modification    Treatment   Treatment
    ## 374             Control      Grill Modification    Treatment   Treatment
    ## 375             Control      Grill Modification    Treatment   Treatment
    ## 376             Control      Grill Modification    Treatment   Treatment
    ## 377             Control        Wok         Main    Satellite   Satellite
    ## 378             Control        Wok         Main    Satellite   Satellite
    ## 379             Control      Ramen         Main    Treatment   Treatment
    ## 380             Control        Wok         Main    Satellite   Satellite
    ## 381             Control      Ramen         Main    Treatment   Treatment
    ## 382             Control        Wok         Side    Satellite   Satellite
    ## 383             Control        Wok         Main    Satellite   Satellite
    ## 384             Control        Wok         Side    Satellite   Satellite
    ## 385             Control        Wok         Side    Satellite   Satellite
    ## 386             Control        Wok         Side    Satellite   Satellite
    ## 387             Control  Breakfast         Main    Satellite   Satellite
    ## 388             Control  Breakfast         Side    Satellite   Satellite
    ## 389             Control  Breakfast         Main    Satellite   Satellite
    ## 390             Control  Breakfast         Main    Satellite   Satellite
    ## 391             Control  Breakfast         Main    Satellite   Satellite
    ## 392             Control  Breakfast Modification    Satellite   Satellite
    ## 393             Control  Breakfast         Side    Satellite   Satellite
    ## 394             Control  Breakfast         Side    Satellite   Satellite
    ## 395             Control  Breakfast         Side    Satellite   Satellite
    ## 396             Control  Breakfast         Side    Satellite   Satellite
    ## 397             Control      Pasta         Main    Satellite   Satellite
    ## 398             Control      Pizza         Main    Satellite   Satellite
    ## 399             Control      Pasta         Main    Satellite   Satellite
    ## 400             Control      Pizza         Main    Satellite   Satellite
    ## 401             Control      Pasta Modification    Satellite   Satellite
    ## 402             Control       Wrap         Main    Satellite   Satellite
    ## 403             Control       Wrap         Main    Satellite   Satellite
    ## 404             Control       Wrap         Side    Satellite   Satellite
    ## 405             Control       Wrap Modification    Satellite   Satellite
    ## 406             Control  Salad Bar         Main    Satellite   Satellite
    ## 407             Control       Soup         Main    Satellite   Satellite
    ## 408             Control       Soup         Main    Satellite   Satellite
    ## 409             Control  Grab N Go         Side    Satellite   Satellite
    ## 410        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 411        Carbon Label      Grill         Main    Treatment   Treatment
    ## 412        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 413        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 414        Carbon Label      Grill         Side    Treatment   Treatment
    ## 415        Carbon Label      Grill         Main    Treatment   Treatment
    ## 416        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 417        Carbon Label      Grill         Side    Treatment   Treatment
    ## 418        Carbon Label      Grill         Main    Treatment   Treatment
    ## 419        Carbon Label      Grill         Main    Treatment   Treatment
    ## 420        Carbon Label      Grill Modification    Treatment   Treatment
    ## 421        Carbon Label      Grill         Main    Treatment   Treatment
    ## 422        Carbon Label      Grill Modification    Treatment   Treatment
    ## 423        Carbon Label      Grill Modification    Treatment   Treatment
    ## 424        Carbon Label      Grill Modification    Treatment   Treatment
    ## 425        Carbon Label      Grill Modification    Treatment   Treatment
    ## 426        Carbon Label        Wok         Main    Satellite   Satellite
    ## 427        Carbon Label        Wok         Main    Satellite   Satellite
    ## 428        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 429        Carbon Label        Wok         Main    Satellite   Satellite
    ## 430        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 431        Carbon Label        Wok         Main    Satellite   Satellite
    ## 432        Carbon Label        Wok         Side    Satellite   Satellite
    ## 433        Carbon Label        Wok         Side    Satellite   Satellite
    ## 434        Carbon Label        Wok         Side    Satellite   Satellite
    ## 435        Carbon Label        Wok         Side    Satellite   Satellite
    ## 436        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 437        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 438        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 439        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 440        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 441        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 442        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 443        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 444        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 445        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 446        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 447        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 448        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 449        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 450        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 451        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 452        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 453        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 454        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 455        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 456        Carbon Label       Wrap Modification    Satellite   Satellite
    ## 457        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 458        Carbon Label       Soup         Main    Satellite   Satellite
    ## 459        Carbon Label       Soup         Main    Satellite   Satellite
    ## 460        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 461        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 462        Carbon Label      Grill         Main    Treatment   Treatment
    ## 463        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 464        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 465        Carbon Label      Grill         Side    Treatment   Treatment
    ## 466        Carbon Label      Grill         Main    Treatment   Treatment
    ## 467        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 468        Carbon Label      Grill         Main    Treatment   Treatment
    ## 469        Carbon Label      Grill         Side    Treatment   Treatment
    ## 470        Carbon Label      Grill         Main    Treatment   Treatment
    ## 471        Carbon Label      Grill         Main    Treatment   Treatment
    ## 472        Carbon Label      Grill Modification    Treatment   Treatment
    ## 473        Carbon Label      Grill Modification    Treatment   Treatment
    ## 474        Carbon Label      Grill Modification    Treatment   Treatment
    ## 475        Carbon Label      Grill Modification    Treatment   Treatment
    ## 476        Carbon Label      Grill Modification    Treatment   Treatment
    ## 477        Carbon Label        Wok         Main    Satellite   Satellite
    ## 478        Carbon Label        Wok         Main    Satellite   Satellite
    ## 479        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 480        Carbon Label        Wok         Main    Satellite   Satellite
    ## 481        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 482        Carbon Label        Wok         Side    Satellite   Satellite
    ## 483        Carbon Label        Wok         Main    Satellite   Satellite
    ## 484        Carbon Label        Wok         Side    Satellite   Satellite
    ## 485        Carbon Label        Wok         Side    Satellite   Satellite
    ## 486        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 487        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 488        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 489        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 490        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 491        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 492        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 493        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 494        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 495        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 496        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 497        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 498        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 499        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 500        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 501        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 502        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 503        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 504        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 505        Carbon Label       Wrap Modification    Satellite   Satellite
    ## 506        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 507        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 508        Carbon Label       Soup         Main    Satellite   Satellite
    ## 509        Carbon Label       Soup         Main    Satellite   Satellite
    ## 510        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 511        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 512        Carbon Label      Grill         Main    Treatment   Treatment
    ## 513        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 514        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 515        Carbon Label      Grill         Side    Treatment   Treatment
    ## 516        Carbon Label      Grill         Main    Treatment   Treatment
    ## 517        Carbon Label      Grill         Main    Treatment   Treatment
    ## 518        Carbon Label      Grill         Main    Treatment   Treatment
    ## 519        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 520        Carbon Label      Grill         Main    Treatment   Treatment
    ## 521        Carbon Label      Grill         Side    Treatment   Treatment
    ## 522        Carbon Label      Grill Modification    Treatment   Treatment
    ## 523        Carbon Label      Grill Modification    Treatment   Treatment
    ## 524        Carbon Label      Grill Modification    Treatment   Treatment
    ## 525        Carbon Label      Grill Modification    Treatment   Treatment
    ## 526        Carbon Label      Grill Modification    Treatment   Treatment
    ## 527        Carbon Label        Wok         Main    Satellite   Satellite
    ## 528        Carbon Label        Wok         Main    Satellite   Satellite
    ## 529        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 530        Carbon Label        Wok         Main    Satellite   Satellite
    ## 531        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 532        Carbon Label        Wok         Side    Satellite   Satellite
    ## 533        Carbon Label        Wok         Main    Satellite   Satellite
    ## 534        Carbon Label        Wok         Side    Satellite   Satellite
    ## 535        Carbon Label        Wok         Side    Satellite   Satellite
    ## 536        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 537        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 538        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 539        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 540        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 541        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 542        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 543        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 544        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 545        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 546        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 547        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 548        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 549        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 550        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 551        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 552        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 553        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 554        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 555        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 556        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 557        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 558        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 559        Carbon Label       Soup         Main    Satellite   Satellite
    ## 560        Carbon Label       Soup         Main    Satellite   Satellite
    ## 561        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 562        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 563        Carbon Label      Grill         Main    Treatment   Treatment
    ## 564        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 565        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 566        Carbon Label      Grill         Side    Treatment   Treatment
    ## 567        Carbon Label      Grill         Main    Treatment   Treatment
    ## 568        Carbon Label      Grill         Main    Treatment   Treatment
    ## 569        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 570        Carbon Label      Grill         Side    Treatment   Treatment
    ## 571        Carbon Label      Grill         Main    Treatment   Treatment
    ## 572        Carbon Label      Grill Modification    Treatment   Treatment
    ## 573        Carbon Label      Grill Modification    Treatment   Treatment
    ## 574        Carbon Label      Grill         Main    Treatment   Treatment
    ## 575        Carbon Label      Grill Modification    Treatment   Treatment
    ## 576        Carbon Label      Grill Modification    Treatment   Treatment
    ## 577        Carbon Label      Grill Modification    Treatment   Treatment
    ## 578        Carbon Label        Wok         Main    Satellite   Satellite
    ## 579        Carbon Label        Wok         Main    Satellite   Satellite
    ## 580        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 581        Carbon Label        Wok         Main    Satellite   Satellite
    ## 582        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 583        Carbon Label        Wok         Side    Satellite   Satellite
    ## 584        Carbon Label        Wok         Main    Satellite   Satellite
    ## 585        Carbon Label        Wok         Side    Satellite   Satellite
    ## 586        Carbon Label        Wok         Side    Satellite   Satellite
    ## 587        Carbon Label        Wok         Side    Satellite   Satellite
    ## 588        Carbon Label        Wok         Side    Satellite   Satellite
    ## 589        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 590        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 591        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 592        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 593        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 594        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 595        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 596        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 597        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 598        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 599        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 600        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 601        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 602        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 603        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 604        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 605        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 606        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 607        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 608        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 609        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 610        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 611        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 612        Carbon Label       Soup         Main    Satellite   Satellite
    ## 613        Carbon Label       Soup         Main    Satellite   Satellite
    ## 614        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 615        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 616        Carbon Label      Grill         Main    Treatment   Treatment
    ## 617        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 618        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 619        Carbon Label      Grill         Side    Treatment   Treatment
    ## 620        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 621        Carbon Label      Grill         Main    Treatment   Treatment
    ## 622        Carbon Label      Grill         Main    Treatment   Treatment
    ## 623        Carbon Label      Grill         Main    Treatment   Treatment
    ## 624        Carbon Label      Grill         Main    Treatment   Treatment
    ## 625        Carbon Label      Grill Modification    Treatment   Treatment
    ## 626        Carbon Label      Grill         Side    Treatment   Treatment
    ## 627        Carbon Label      Grill Modification    Treatment   Treatment
    ## 628        Carbon Label      Grill Modification    Treatment   Treatment
    ## 629        Carbon Label      Grill Modification    Treatment   Treatment
    ## 630        Carbon Label        Wok         Main    Satellite   Satellite
    ## 631        Carbon Label        Wok         Main    Satellite   Satellite
    ## 632        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 633        Carbon Label        Wok         Main    Satellite   Satellite
    ## 634        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 635        Carbon Label        Wok         Side    Satellite   Satellite
    ## 636        Carbon Label        Wok         Main    Satellite   Satellite
    ## 637        Carbon Label        Wok         Side    Satellite   Satellite
    ## 638        Carbon Label        Wok         Side    Satellite   Satellite
    ## 639        Carbon Label        Wok         Side    Satellite   Satellite
    ## 640        Carbon Label        Wok         Side    Satellite   Satellite
    ## 641        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 642        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 643        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 644        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 645        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 646        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 647        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 648        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 649        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 650        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 651        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 652        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 653        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 654        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 655        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 656        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 657        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 658        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 659        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 660        Carbon Label       Soup         Main    Satellite   Satellite
    ## 661        Carbon Label       Soup         Main    Satellite   Satellite
    ## 662        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 663        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 664        Carbon Label      Grill         Main    Treatment   Treatment
    ## 665        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 666        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 667        Carbon Label      Grill         Side    Treatment   Treatment
    ## 668        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 669        Carbon Label      Grill         Main    Treatment   Treatment
    ## 670        Carbon Label      Grill         Main    Treatment   Treatment
    ## 671        Carbon Label      Grill         Main    Treatment   Treatment
    ## 672        Carbon Label      Grill Modification    Treatment   Treatment
    ## 673        Carbon Label      Grill         Main    Treatment   Treatment
    ## 674        Carbon Label      Grill Modification    Treatment   Treatment
    ## 675        Carbon Label      Grill Modification    Treatment   Treatment
    ## 676        Carbon Label      Grill Modification    Treatment   Treatment
    ## 677        Carbon Label      Grill Modification    Treatment   Treatment
    ## 678        Carbon Label        Wok         Main    Satellite   Satellite
    ## 679        Carbon Label        Wok         Main    Satellite   Satellite
    ## 680        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 681        Carbon Label        Wok         Main    Satellite   Satellite
    ## 682        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 683        Carbon Label        Wok         Main    Satellite   Satellite
    ## 684        Carbon Label        Wok         Side    Satellite   Satellite
    ## 685        Carbon Label        Wok         Side    Satellite   Satellite
    ## 686        Carbon Label        Wok         Side    Satellite   Satellite
    ## 687        Carbon Label        Wok         Side    Satellite   Satellite
    ## 688        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 689        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 690        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 691        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 692        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 693        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 694        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 695        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 696        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 697        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 698        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 699        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 700        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 701        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 702        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 703        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 704        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 705        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 706        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 707        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 708        Carbon Label       Wrap Modification    Satellite   Satellite
    ## 709        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 710        Carbon Label       Soup         Main    Satellite   Satellite
    ## 711        Carbon Label       Soup         Main    Satellite   Satellite
    ## 712        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 713        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 714        Carbon Label      Grill         Main    Treatment   Treatment
    ## 715        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 716        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 717        Carbon Label      Grill         Side    Treatment   Treatment
    ## 718        Carbon Label      Grill         Main    Treatment   Treatment
    ## 719        Carbon Label      Grill         Main    Treatment   Treatment
    ## 720        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 721        Carbon Label      Grill         Main    Treatment   Treatment
    ## 722        Carbon Label      Grill         Side    Treatment   Treatment
    ## 723        Carbon Label      Grill Modification    Treatment   Treatment
    ## 724        Carbon Label      Grill         Main    Treatment   Treatment
    ## 725        Carbon Label      Grill Modification    Treatment   Treatment
    ## 726        Carbon Label      Grill Modification    Treatment   Treatment
    ## 727        Carbon Label      Grill Modification    Treatment   Treatment
    ## 728        Carbon Label      Grill Modification    Treatment   Treatment
    ## 729        Carbon Label        Wok         Main    Satellite   Satellite
    ## 730        Carbon Label        Wok         Main    Satellite   Satellite
    ## 731        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 732        Carbon Label        Wok         Main    Satellite   Satellite
    ## 733        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 734        Carbon Label        Wok         Side    Satellite   Satellite
    ## 735        Carbon Label        Wok         Main    Satellite   Satellite
    ## 736        Carbon Label        Wok         Side    Satellite   Satellite
    ## 737        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 738        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 739        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 740        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 741        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 742        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 743        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 744        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 745        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 746        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 747        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 748        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 749        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 750        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 751        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 752        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 753        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 754        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 755        Carbon Label       Wrap Modification    Satellite   Satellite
    ## 756        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 757        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 758        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 759        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 760        Carbon Label       Soup         Main    Satellite   Satellite
    ## 761        Carbon Label       Soup         Main    Satellite   Satellite
    ## 762        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 763        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 764        Carbon Label      Grill         Main    Treatment   Treatment
    ## 765        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 766        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 767        Carbon Label      Grill         Side    Treatment   Treatment
    ## 768        Carbon Label      Grill         Main    Treatment   Treatment
    ## 769        Carbon Label      Grill         Side    Treatment   Treatment
    ## 770        Carbon Label      Grill         Main    Treatment   Treatment
    ## 771        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 772        Carbon Label      Grill         Main    Treatment   Treatment
    ## 773        Carbon Label      Grill Modification    Treatment   Treatment
    ## 774        Carbon Label      Grill Modification    Treatment   Treatment
    ## 775        Carbon Label      Grill         Main    Treatment   Treatment
    ## 776        Carbon Label      Grill Modification    Treatment   Treatment
    ## 777        Carbon Label      Grill Modification    Treatment   Treatment
    ## 778        Carbon Label      Grill Modification    Treatment   Treatment
    ## 779        Carbon Label        Wok         Main    Satellite   Satellite
    ## 780        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 781        Carbon Label        Wok         Main    Satellite   Satellite
    ## 782        Carbon Label        Wok         Main    Satellite   Satellite
    ## 783        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 784        Carbon Label        Wok         Main    Satellite   Satellite
    ## 785        Carbon Label        Wok         Side    Satellite   Satellite
    ## 786        Carbon Label        Wok         Side    Satellite   Satellite
    ## 787        Carbon Label        Wok         Side    Satellite   Satellite
    ## 788        Carbon Label        Wok         Side    Satellite   Satellite
    ## 789        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 790        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 791        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 792        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 793        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 794        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 795        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 796        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 797        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 798        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 799        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 800        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 801        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 802        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 803        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 804        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 805        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 806        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 807        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 808        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 809        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 810        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 811        Carbon Label       Soup         Main    Satellite   Satellite
    ## 812        Carbon Label       Soup         Main    Satellite   Satellite
    ## 813        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 814        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 815        Carbon Label      Grill         Main    Treatment   Treatment
    ## 816        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 817        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 818        Carbon Label      Grill         Side    Treatment   Treatment
    ## 819        Carbon Label      Grill         Main    Treatment   Treatment
    ## 820        Carbon Label      Grill         Main    Treatment   Treatment
    ## 821        Carbon Label      Grill         Side    Treatment   Treatment
    ## 822        Carbon Label      Grill         Main    Treatment   Treatment
    ## 823        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 824        Carbon Label      Grill Modification    Treatment   Treatment
    ## 825        Carbon Label      Grill         Main    Treatment   Treatment
    ## 826        Carbon Label      Grill Modification    Treatment   Treatment
    ## 827        Carbon Label      Grill Modification    Treatment   Treatment
    ## 828        Carbon Label      Grill Modification    Treatment   Treatment
    ## 829        Carbon Label      Grill Modification    Treatment   Treatment
    ## 830        Carbon Label        Wok         Main    Satellite   Satellite
    ## 831        Carbon Label        Wok         Main    Satellite   Satellite
    ## 832        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 833        Carbon Label        Wok         Main    Satellite   Satellite
    ## 834        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 835        Carbon Label        Wok         Side    Satellite   Satellite
    ## 836        Carbon Label        Wok         Main    Satellite   Satellite
    ## 837        Carbon Label        Wok         Side    Satellite   Satellite
    ## 838        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 839        Carbon Label        Wok         Side    Satellite   Satellite
    ## 840        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 841        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 842        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 843        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 844        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 845        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 846        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 847        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 848        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 849        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 850        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 851        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 852        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 853        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 854        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 855        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 856        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 857        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 858        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 859        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 860        Carbon Label       Wrap Modification    Satellite   Satellite
    ## 861        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 862        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 863        Carbon Label       Soup         Main    Satellite   Satellite
    ## 864        Carbon Label       Soup         Main    Satellite   Satellite
    ## 865        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 866        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 867        Carbon Label      Grill         Main    Treatment   Treatment
    ## 868        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 869        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 870        Carbon Label      Grill         Side    Treatment   Treatment
    ## 871        Carbon Label      Grill         Main    Treatment   Treatment
    ## 872        Carbon Label Quesadilla         Main    Satellite   Satellite
    ## 873        Carbon Label      Grill         Side    Treatment   Treatment
    ## 874        Carbon Label      Grill         Main    Treatment   Treatment
    ## 875        Carbon Label      Grill         Main    Treatment   Treatment
    ## 876        Carbon Label      Grill Modification    Treatment   Treatment
    ## 877        Carbon Label      Grill Modification    Treatment   Treatment
    ## 878        Carbon Label      Grill         Main    Treatment   Treatment
    ## 879        Carbon Label      Grill Modification    Treatment   Treatment
    ## 880        Carbon Label      Grill Modification    Treatment   Treatment
    ## 881        Carbon Label      Grill Modification    Treatment   Treatment
    ## 882        Carbon Label        Wok         Main    Satellite   Satellite
    ## 883        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 884        Carbon Label        Wok         Main    Satellite   Satellite
    ## 885        Carbon Label        Wok         Main    Satellite   Satellite
    ## 886        Carbon Label      Ramen         Main    Treatment   Treatment
    ## 887        Carbon Label        Wok         Side    Satellite   Satellite
    ## 888        Carbon Label        Wok         Main    Satellite   Satellite
    ## 889        Carbon Label        Wok         Side    Satellite   Satellite
    ## 890        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 891        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 892        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 893        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 894        Carbon Label  Breakfast         Main    Satellite   Satellite
    ## 895        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 896        Carbon Label  Breakfast Modification    Satellite   Satellite
    ## 897        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 898        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 899        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 900        Carbon Label  Breakfast         Side    Satellite   Satellite
    ## 901        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 902        Carbon Label      Pasta         Main    Satellite   Satellite
    ## 903        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 904        Carbon Label      Pizza         Main    Satellite   Satellite
    ## 905        Carbon Label      Pasta Modification    Satellite   Satellite
    ## 906        Carbon Label      Pasta         Side    Satellite   Satellite
    ## 907        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 908        Carbon Label       Wrap         Side    Satellite   Satellite
    ## 909        Carbon Label       Wrap         Main    Satellite   Satellite
    ## 910        Carbon Label  Salad Bar         Main    Satellite   Satellite
    ## 911        Carbon Label  Salad Bar Modification    Satellite   Satellite
    ## 912        Carbon Label       Soup         Main    Satellite   Satellite
    ## 913        Carbon Label       Soup         Main    Satellite   Satellite
    ## 914        Carbon Label  Grab N Go         Side    Satellite   Satellite
    ## 915             Default Quesadilla         Main    Satellite   Satellite
    ## 916             Default      Grill         Main    Treatment   Treatment
    ## 917             Default  Grab N Go         Side    Satellite   Satellite
    ## 918             Default Quesadilla         Main    Satellite   Satellite
    ## 919             Default      Grill         Side    Treatment   Treatment
    ## 920             Default      Grill         Main    Treatment   Treatment
    ## 921             Default Quesadilla         Main    Satellite   Satellite
    ## 922             Default      Grill         Side    Treatment   Treatment
    ## 923             Default      Grill         Main    Treatment   Treatment
    ## 924             Default      Grill         Main    Treatment   Treatment
    ## 925             Default      Grill Modification    Treatment   Treatment
    ## 926             Default      Grill         Main    Treatment   Treatment
    ## 927             Default      Grill Modification    Treatment   Treatment
    ## 928             Default      Grill Modification    Treatment   Treatment
    ## 929             Default      Grill Modification    Treatment   Treatment
    ## 930             Default      Grill Modification    Treatment   Treatment
    ## 931             Default        Wok         Main    Satellite   Satellite
    ## 932             Default        Wok         Main    Satellite   Satellite
    ## 933             Default      Ramen         Main    Treatment   Treatment
    ## 934             Default        Wok         Main    Satellite   Satellite
    ## 935             Default      Ramen         Main    Treatment   Treatment
    ## 936             Default        Wok         Side    Satellite   Satellite
    ## 937             Default        Wok         Main    Satellite   Satellite
    ## 938             Default        Wok         Side    Satellite   Satellite
    ## 939             Default        Wok         Side    Satellite   Satellite
    ## 940             Default        Wok         Side    Satellite   Satellite
    ## 941             Default        Wok         Side    Satellite   Satellite
    ## 942             Default      Pasta         Main    Satellite   Satellite
    ## 943             Default      Pasta         Main    Satellite   Satellite
    ## 944             Default      Pizza         Main    Satellite   Satellite
    ## 945             Default      Pizza         Main    Satellite   Satellite
    ## 946             Default      Pasta Modification    Satellite   Satellite
    ## 947             Default      Pasta         Side    Satellite   Satellite
    ## 948             Default  Breakfast         Main    Satellite   Satellite
    ## 949             Default  Breakfast         Side    Satellite   Satellite
    ## 950             Default  Breakfast         Main    Satellite   Satellite
    ## 951             Default  Breakfast         Main    Satellite   Satellite
    ## 952             Default  Breakfast         Main    Satellite   Satellite
    ## 953             Default  Breakfast Modification    Satellite   Satellite
    ## 954             Default  Breakfast         Side    Satellite   Satellite
    ## 955             Default  Breakfast         Side    Satellite   Satellite
    ## 956             Default  Breakfast         Side    Satellite   Satellite
    ## 957             Default  Breakfast         Side    Satellite   Satellite
    ## 958             Default  Breakfast         Side    Satellite   Satellite
    ## 959             Default       Wrap         Main    Satellite   Satellite
    ## 960             Default       Wrap         Main    Satellite   Satellite
    ## 961             Default  Salad Bar         Main    Satellite   Satellite
    ## 962             Default       Soup         Main    Satellite   Satellite
    ## 963             Default       Soup         Main    Satellite   Satellite
    ## 964             Default  Grab N Go         Side    Satellite   Satellite
    ## 965             Default Quesadilla         Main    Satellite   Satellite
    ## 966             Default      Grill         Main    Treatment   Treatment
    ## 967             Default  Grab N Go         Side    Satellite   Satellite
    ## 968             Default Quesadilla         Main    Satellite   Satellite
    ## 969             Default      Grill         Side    Treatment   Treatment
    ## 970             Default      Grill         Main    Treatment   Treatment
    ## 971             Default Quesadilla         Main    Satellite   Satellite
    ## 972             Default      Grill         Main    Treatment   Treatment
    ## 973             Default      Grill         Main    Treatment   Treatment
    ## 974             Default      Grill Modification    Treatment   Treatment
    ## 975             Default      Grill         Main    Treatment   Treatment
    ## 976             Default      Grill Modification    Treatment   Treatment
    ## 977             Default      Grill Modification    Treatment   Treatment
    ## 978             Default      Grill Modification    Treatment   Treatment
    ## 979             Default      Grill Modification    Treatment   Treatment
    ## 980             Default        Wok         Main    Satellite   Satellite
    ## 981             Default        Wok         Main    Satellite   Satellite
    ## 982             Default      Ramen         Main    Treatment   Treatment
    ## 983             Default        Wok         Main    Satellite   Satellite
    ## 984             Default      Ramen         Main    Treatment   Treatment
    ## 985             Default        Wok         Main    Satellite   Satellite
    ## 986             Default        Wok         Side    Satellite   Satellite
    ## 987             Default        Wok         Side    Satellite   Satellite
    ## 988             Default      Ramen         Main    Treatment   Treatment
    ## 989             Default        Wok         Side    Satellite   Satellite
    ## 990             Default        Wok         Side    Satellite   Satellite
    ## 991             Default  Breakfast         Main    Satellite   Satellite
    ## 992             Default  Breakfast         Side    Satellite   Satellite
    ## 993             Default  Breakfast         Main    Satellite   Satellite
    ## 994             Default  Breakfast         Main    Satellite   Satellite
    ## 995             Default  Breakfast         Main    Satellite   Satellite
    ## 996             Default  Breakfast Modification    Satellite   Satellite
    ## 997             Default  Breakfast         Side    Satellite   Satellite
    ## 998             Default  Breakfast         Side    Satellite   Satellite
    ## 999             Default  Breakfast         Side    Satellite   Satellite
    ## 1000            Default  Breakfast         Side    Satellite   Satellite
    ## 1001            Default      Pasta         Main    Satellite   Satellite
    ## 1002            Default      Pizza         Main    Satellite   Satellite
    ## 1003            Default      Pasta         Main    Satellite   Satellite
    ## 1004            Default      Pizza         Main    Satellite   Satellite
    ## 1005            Default      Pasta Modification    Satellite   Satellite
    ## 1006            Default       Wrap         Main    Satellite   Satellite
    ## 1007            Default       Wrap         Main    Satellite   Satellite
    ## 1008            Default       Wrap         Side    Satellite   Satellite
    ## 1009            Default       Wrap Modification    Satellite   Satellite
    ## 1010            Default  Salad Bar         Main    Satellite   Satellite
    ## 1011            Default  Salad Bar Modification    Satellite   Satellite
    ## 1012            Default       Soup         Main    Satellite   Satellite
    ## 1013            Default       Soup         Main    Satellite   Satellite
    ## 1014            Default  Grab N Go         Side    Satellite   Satellite
    ## 1015            Default Quesadilla         Main    Satellite   Satellite
    ## 1016            Default      Grill         Main    Treatment   Treatment
    ## 1017            Default  Grab N Go         Side    Satellite   Satellite
    ## 1018            Default Quesadilla         Main    Satellite   Satellite
    ## 1019            Default      Grill         Side    Treatment   Treatment
    ## 1020            Default      Grill         Main    Treatment   Treatment
    ## 1021            Default Quesadilla         Main    Satellite   Satellite
    ## 1022            Default      Grill         Main    Treatment   Treatment
    ## 1023            Default      Grill         Main    Treatment   Treatment
    ## 1024            Default      Grill Modification    Treatment   Treatment
    ## 1025            Default      Grill Modification    Treatment   Treatment
    ## 1026            Default      Grill         Main    Treatment   Treatment
    ## 1027            Default      Grill Modification    Treatment   Treatment
    ## 1028            Default      Grill Modification    Treatment   Treatment
    ## 1029            Default      Grill Modification    Treatment   Treatment
    ## 1030            Default        Wok         Main    Satellite   Satellite
    ## 1031            Default        Wok         Main    Satellite   Satellite
    ## 1032            Default      Ramen         Main    Treatment   Treatment
    ## 1033            Default        Wok         Main    Satellite   Satellite
    ## 1034            Default      Ramen         Main    Treatment   Treatment
    ## 1035            Default        Wok         Main    Satellite   Satellite
    ## 1036            Default        Wok         Side    Satellite   Satellite
    ## 1037            Default        Wok         Side    Satellite   Satellite
    ## 1038            Default        Wok         Side    Satellite   Satellite
    ## 1039            Default        Wok         Side    Satellite   Satellite
    ## 1040            Default        Wok         Side    Satellite   Satellite
    ## 1041            Default      Pasta         Main    Satellite   Satellite
    ## 1042            Default      Pasta         Main    Satellite   Satellite
    ## 1043            Default      Pizza         Main    Satellite   Satellite
    ## 1044            Default      Pizza         Main    Satellite   Satellite
    ## 1045            Default      Pasta Modification    Satellite   Satellite
    ## 1046            Default      Pasta         Side    Satellite   Satellite
    ## 1047            Default  Breakfast         Main    Satellite   Satellite
    ## 1048            Default  Breakfast         Side    Satellite   Satellite
    ## 1049            Default  Breakfast         Main    Satellite   Satellite
    ## 1050            Default  Breakfast         Main    Satellite   Satellite
    ## 1051            Default  Breakfast         Main    Satellite   Satellite
    ## 1052            Default  Breakfast Modification    Satellite   Satellite
    ## 1053            Default  Breakfast         Side    Satellite   Satellite
    ## 1054            Default  Breakfast         Side    Satellite   Satellite
    ## 1055            Default  Breakfast         Side    Satellite   Satellite
    ## 1056            Default  Breakfast         Side    Satellite   Satellite
    ## 1057            Default       Wrap         Main    Satellite   Satellite
    ## 1058            Default       Wrap         Main    Satellite   Satellite
    ## 1059            Default  Salad Bar         Main    Satellite   Satellite
    ## 1060            Default  Salad Bar Modification    Satellite   Satellite
    ## 1061            Default       Soup         Main    Satellite   Satellite
    ## 1062            Default       Soup         Main    Satellite   Satellite
    ## 1063            Default  Grab N Go         Side    Satellite   Satellite
    ## 1064            Default Quesadilla         Main    Satellite   Satellite
    ## 1065            Default      Grill         Main    Treatment   Treatment
    ## 1066            Default  Grab N Go         Side    Satellite   Satellite
    ## 1067            Default Quesadilla         Main    Satellite   Satellite
    ## 1068            Default      Grill         Side    Treatment   Treatment
    ## 1069            Default      Grill         Main    Treatment   Treatment
    ## 1070            Default      Grill         Main    Treatment   Treatment
    ## 1071            Default Quesadilla         Main    Satellite   Satellite
    ## 1072            Default      Grill         Main    Treatment   Treatment
    ## 1073            Default      Grill Modification    Treatment   Treatment
    ## 1074            Default      Grill         Main    Treatment   Treatment
    ## 1075            Default      Grill Modification    Treatment   Treatment
    ## 1076            Default      Grill Modification    Treatment   Treatment
    ## 1077            Default      Grill         Side    Treatment   Treatment
    ## 1078            Default      Grill Modification    Treatment   Treatment
    ## 1079            Default        Wok         Main    Satellite   Satellite
    ## 1080            Default        Wok         Main    Satellite   Satellite
    ## 1081            Default      Ramen         Main    Treatment   Treatment
    ## 1082            Default        Wok         Main    Satellite   Satellite
    ## 1083            Default      Ramen         Main    Treatment   Treatment
    ## 1084            Default        Wok         Side    Satellite   Satellite
    ## 1085            Default        Wok         Main    Satellite   Satellite
    ## 1086            Default        Wok         Side    Satellite   Satellite
    ## 1087            Default        Wok         Side    Satellite   Satellite
    ## 1088            Default        Wok         Side    Satellite   Satellite
    ## 1089            Default  Breakfast         Main    Satellite   Satellite
    ## 1090            Default  Breakfast         Side    Satellite   Satellite
    ## 1091            Default  Breakfast         Main    Satellite   Satellite
    ## 1092            Default  Breakfast         Main    Satellite   Satellite
    ## 1093            Default  Breakfast         Main    Satellite   Satellite
    ## 1094            Default  Breakfast Modification    Satellite   Satellite
    ## 1095            Default  Breakfast         Side    Satellite   Satellite
    ## 1096            Default  Breakfast         Side    Satellite   Satellite
    ## 1097            Default  Breakfast         Side    Satellite   Satellite
    ## 1098            Default  Breakfast         Side    Satellite   Satellite
    ## 1099            Default      Pasta         Main    Satellite   Satellite
    ## 1100            Default      Pasta         Main    Satellite   Satellite
    ## 1101            Default      Pizza         Main    Satellite   Satellite
    ## 1102            Default      Pizza         Main    Satellite   Satellite
    ## 1103            Default      Pasta Modification    Satellite   Satellite
    ## 1104            Default       Wrap         Main    Satellite   Satellite
    ## 1105            Default       Wrap         Main    Satellite   Satellite
    ## 1106            Default       Wrap         Side    Satellite   Satellite
    ## 1107            Default       Wrap         Side    Satellite   Satellite
    ## 1108            Default  Salad Bar         Main    Satellite   Satellite
    ## 1109            Default  Salad Bar Modification    Satellite   Satellite
    ## 1110            Default       Soup         Main    Satellite   Satellite
    ## 1111            Default       Soup         Main    Satellite   Satellite
    ## 1112            Default  Grab N Go         Side    Satellite   Satellite
    ## 1113            Default Quesadilla         Main    Satellite   Satellite
    ## 1114            Default      Grill         Main    Treatment   Treatment
    ## 1115            Default Quesadilla         Main    Satellite   Satellite
    ## 1116            Default  Grab N Go         Side    Satellite   Satellite
    ## 1117            Default      Grill         Side    Treatment   Treatment
    ## 1118            Default Quesadilla         Main    Satellite   Satellite
    ## 1119            Default      Grill         Main    Treatment   Treatment
    ## 1120            Default      Grill         Main    Treatment   Treatment
    ## 1121            Default      Grill         Main    Treatment   Treatment
    ## 1122            Default      Grill         Main    Treatment   Treatment
    ## 1123            Default      Grill Modification    Treatment   Treatment
    ## 1124            Default      Grill Modification    Treatment   Treatment
    ## 1125            Default      Grill Modification    Treatment   Treatment
    ## 1126            Default      Grill Modification    Treatment   Treatment
    ## 1127            Default      Grill Modification    Treatment   Treatment
    ## 1128            Default        Wok         Main    Satellite   Satellite
    ## 1129            Default        Wok         Main    Satellite   Satellite
    ## 1130            Default      Ramen         Main    Treatment   Treatment
    ## 1131            Default        Wok         Main    Satellite   Satellite
    ## 1132            Default      Ramen         Main    Treatment   Treatment
    ## 1133            Default        Wok         Side    Satellite   Satellite
    ## 1134            Default        Wok         Main    Satellite   Satellite
    ## 1135            Default        Wok         Side    Satellite   Satellite
    ## 1136            Default        Wok         Side    Satellite   Satellite
    ## 1137            Default        Wok         Side    Satellite   Satellite
    ## 1138            Default        Wok         Side    Satellite   Satellite
    ## 1139            Default  Breakfast         Main    Satellite   Satellite
    ## 1140            Default  Breakfast         Side    Satellite   Satellite
    ## 1141            Default  Breakfast         Main    Satellite   Satellite
    ## 1142            Default  Breakfast         Main    Satellite   Satellite
    ## 1143            Default  Breakfast         Main    Satellite   Satellite
    ## 1144            Default  Breakfast Modification    Satellite   Satellite
    ## 1145            Default  Breakfast         Side    Satellite   Satellite
    ## 1146            Default  Breakfast         Side    Satellite   Satellite
    ## 1147            Default  Breakfast         Side    Satellite   Satellite
    ## 1148            Default  Breakfast         Side    Satellite   Satellite
    ## 1149            Default      Pasta         Main    Satellite   Satellite
    ## 1150            Default      Pizza         Main    Satellite   Satellite
    ## 1151            Default      Pasta         Main    Satellite   Satellite
    ## 1152            Default      Pizza         Main    Satellite   Satellite
    ## 1153            Default      Pasta Modification    Satellite   Satellite
    ## 1154            Default      Pasta         Side    Satellite   Satellite
    ## 1155            Default       Wrap         Main    Satellite   Satellite
    ## 1156            Default       Wrap         Main    Satellite   Satellite
    ## 1157            Default       Wrap         Side    Satellite   Satellite
    ## 1158            Default  Salad Bar         Main    Satellite   Satellite
    ## 1159            Default       Soup         Main    Satellite   Satellite
    ## 1160            Default       Soup         Main    Satellite   Satellite
    ## 1161            Default  Grab N Go         Side    Satellite   Satellite
    ## 1162            Default Quesadilla         Main    Satellite   Satellite
    ## 1163            Default      Grill         Main    Treatment   Treatment
    ## 1164            Default  Grab N Go         Side    Satellite   Satellite
    ## 1165            Default Quesadilla         Main    Satellite   Satellite
    ## 1166            Default      Grill         Side    Treatment   Treatment
    ## 1167            Default      Grill         Main    Treatment   Treatment
    ## 1168            Default      Grill         Main    Treatment   Treatment
    ## 1169            Default      Grill         Main    Treatment   Treatment
    ## 1170            Default      Grill         Main    Treatment   Treatment
    ## 1171            Default      Grill Modification    Treatment   Treatment
    ## 1172            Default Quesadilla         Main    Satellite   Satellite
    ## 1173            Default      Grill Modification    Treatment   Treatment
    ## 1174            Default      Grill Modification    Treatment   Treatment
    ## 1175            Default      Grill Modification    Treatment   Treatment
    ## 1176            Default      Grill Modification    Treatment   Treatment
    ## 1177            Default        Wok         Main    Satellite   Satellite
    ## 1178            Default        Wok         Main    Satellite   Satellite
    ## 1179            Default      Ramen         Main    Treatment   Treatment
    ## 1180            Default        Wok         Main    Satellite   Satellite
    ## 1181            Default      Ramen         Main    Treatment   Treatment
    ## 1182            Default        Wok         Main    Satellite   Satellite
    ## 1183            Default        Wok         Side    Satellite   Satellite
    ## 1184            Default        Wok         Side    Satellite   Satellite
    ## 1185            Default        Wok         Side    Satellite   Satellite
    ## 1186            Default        Wok         Side    Satellite   Satellite
    ## 1187            Default      Pasta         Main    Satellite   Satellite
    ## 1188            Default      Pasta         Main    Satellite   Satellite
    ## 1189            Default      Pizza         Main    Satellite   Satellite
    ## 1190            Default      Pizza         Main    Satellite   Satellite
    ## 1191            Default      Pasta Modification    Satellite   Satellite
    ## 1192            Default  Breakfast         Main    Satellite   Satellite
    ## 1193            Default  Breakfast         Side    Satellite   Satellite
    ## 1194            Default  Breakfast         Main    Satellite   Satellite
    ## 1195            Default  Breakfast         Main    Satellite   Satellite
    ## 1196            Default  Breakfast         Main    Satellite   Satellite
    ## 1197            Default  Breakfast Modification    Satellite   Satellite
    ## 1198            Default  Breakfast         Side    Satellite   Satellite
    ## 1199            Default  Breakfast         Side    Satellite   Satellite
    ## 1200            Default  Breakfast         Side    Satellite   Satellite
    ## 1201            Default  Breakfast         Side    Satellite   Satellite
    ## 1202            Default  Breakfast         Side    Satellite   Satellite
    ## 1203            Default  Breakfast         Side    Satellite   Satellite
    ## 1204            Default       Wrap         Main    Satellite   Satellite
    ## 1205            Default       Wrap         Main    Satellite   Satellite
    ## 1206            Default       Wrap         Side    Satellite   Satellite
    ## 1207            Default       Wrap         Side    Satellite   Satellite
    ## 1208            Default  Salad Bar         Main    Satellite   Satellite
    ## 1209            Default       Soup         Main    Satellite   Satellite
    ## 1210            Default       Soup         Main    Satellite   Satellite
    ## 1211            Default  Grab N Go         Side    Satellite   Satellite
    ## 1212            Default Quesadilla         Main    Satellite   Satellite
    ## 1213            Default      Grill         Main    Treatment   Treatment
    ## 1214            Default  Grab N Go         Side    Satellite   Satellite
    ## 1215            Default Quesadilla         Main    Satellite   Satellite
    ## 1216            Default      Grill         Side    Treatment   Treatment
    ## 1217            Default      Grill         Main    Treatment   Treatment
    ## 1218            Default Quesadilla         Main    Satellite   Satellite
    ## 1219            Default      Grill         Main    Treatment   Treatment
    ## 1220            Default      Grill         Side    Treatment   Treatment
    ## 1221            Default      Grill         Main    Treatment   Treatment
    ## 1222            Default      Grill         Main    Treatment   Treatment
    ## 1223            Default      Grill Modification    Treatment   Treatment
    ## 1224            Default      Grill Modification    Treatment   Treatment
    ## 1225            Default      Grill Modification    Treatment   Treatment
    ## 1226            Default      Grill Modification    Treatment   Treatment
    ## 1227            Default      Grill Modification    Treatment   Treatment
    ## 1228            Default        Wok         Main    Satellite   Satellite
    ## 1229            Default        Wok         Main    Satellite   Satellite
    ## 1230            Default      Ramen         Main    Treatment   Treatment
    ## 1231            Default        Wok         Main    Satellite   Satellite
    ## 1232            Default      Ramen         Main    Treatment   Treatment
    ## 1233            Default        Wok         Side    Satellite   Satellite
    ## 1234            Default        Wok         Main    Satellite   Satellite
    ## 1235            Default        Wok         Side    Satellite   Satellite
    ## 1236            Default        Wok         Side    Satellite   Satellite
    ## 1237            Default        Wok         Side    Satellite   Satellite
    ## 1238            Default      Pasta         Main    Satellite   Satellite
    ## 1239            Default      Pasta         Main    Satellite   Satellite
    ## 1240            Default      Pizza         Main    Satellite   Satellite
    ## 1241            Default      Pizza         Main    Satellite   Satellite
    ## 1242            Default      Pasta Modification    Satellite   Satellite
    ## 1243            Default  Breakfast         Main    Satellite   Satellite
    ## 1244            Default  Breakfast         Side    Satellite   Satellite
    ## 1245            Default  Breakfast         Main    Satellite   Satellite
    ## 1246            Default  Breakfast         Main    Satellite   Satellite
    ## 1247            Default  Breakfast         Main    Satellite   Satellite
    ## 1248            Default  Breakfast Modification    Satellite   Satellite
    ## 1249            Default  Breakfast         Side    Satellite   Satellite
    ## 1250            Default  Breakfast         Side    Satellite   Satellite
    ## 1251            Default  Breakfast         Side    Satellite   Satellite
    ## 1252            Default  Breakfast         Side    Satellite   Satellite
    ## 1253            Default       Wrap         Main    Satellite   Satellite
    ## 1254            Default       Wrap         Main    Satellite   Satellite
    ## 1255            Default       Wrap         Side    Satellite   Satellite
    ## 1256            Default       Wrap Modification    Satellite   Satellite
    ## 1257            Default  Salad Bar         Main    Satellite   Satellite
    ## 1258            Default  Salad Bar Modification    Satellite   Satellite
    ## 1259            Default       Soup         Main    Satellite   Satellite
    ## 1260            Default       Soup         Main    Satellite   Satellite
    ## 1261            Default  Grab N Go         Side    Satellite   Satellite
    ## 1262            Default Quesadilla         Main    Satellite   Satellite
    ## 1263            Default      Grill         Main    Treatment   Treatment
    ## 1264            Default  Grab N Go         Side    Satellite   Satellite
    ## 1265            Default Quesadilla         Main    Satellite   Satellite
    ## 1266            Default      Grill         Side    Treatment   Treatment
    ## 1267            Default      Grill         Main    Treatment   Treatment
    ## 1268            Default      Grill         Main    Treatment   Treatment
    ## 1269            Default Quesadilla         Main    Satellite   Satellite
    ## 1270            Default      Grill         Main    Treatment   Treatment
    ## 1271            Default      Grill         Side    Treatment   Treatment
    ## 1272            Default      Grill Modification    Treatment   Treatment
    ## 1273            Default      Grill         Main    Treatment   Treatment
    ## 1274            Default      Grill Modification    Treatment   Treatment
    ## 1275            Default      Grill Modification    Treatment   Treatment
    ## 1276            Default      Grill Modification    Treatment   Treatment
    ## 1277            Default      Grill Modification    Treatment   Treatment
    ## 1278            Default        Wok         Main    Satellite   Satellite
    ## 1279            Default        Wok         Main    Satellite   Satellite
    ## 1280            Default      Ramen         Main    Treatment   Treatment
    ## 1281            Default        Wok         Main    Satellite   Satellite
    ## 1282            Default      Ramen         Main    Treatment   Treatment
    ## 1283            Default        Wok         Side    Satellite   Satellite
    ## 1284            Default        Wok         Side    Satellite   Satellite
    ## 1285            Default        Wok         Main    Satellite   Satellite
    ## 1286            Default        Wok         Side    Satellite   Satellite
    ## 1287            Default        Wok         Side    Satellite   Satellite
    ## 1288            Default        Wok         Side    Satellite   Satellite
    ## 1289            Default      Pasta         Main    Satellite   Satellite
    ## 1290            Default      Pasta         Main    Satellite   Satellite
    ## 1291            Default      Pasta Modification    Satellite   Satellite
    ## 1292            Default      Pizza         Main    Satellite   Satellite
    ## 1293            Default      Pizza         Main    Satellite   Satellite
    ## 1294            Default      Pasta         Side    Satellite   Satellite
    ## 1295            Default  Breakfast         Main    Satellite   Satellite
    ## 1296            Default  Breakfast         Side    Satellite   Satellite
    ## 1297            Default  Breakfast         Main    Satellite   Satellite
    ## 1298            Default  Breakfast         Main    Satellite   Satellite
    ## 1299            Default  Breakfast         Main    Satellite   Satellite
    ## 1300            Default  Breakfast Modification    Satellite   Satellite
    ## 1301            Default  Breakfast         Side    Satellite   Satellite
    ## 1302            Default  Breakfast         Side    Satellite   Satellite
    ## 1303            Default  Breakfast         Side    Satellite   Satellite
    ## 1304            Default  Breakfast         Side    Satellite   Satellite
    ## 1305            Default  Breakfast         Side    Satellite   Satellite
    ## 1306            Default  Breakfast         Side    Satellite   Satellite
    ## 1307            Default       Wrap         Main    Satellite   Satellite
    ## 1308            Default       Wrap         Main    Satellite   Satellite
    ## 1309            Default       Wrap Modification    Satellite   Satellite
    ## 1310            Default  Salad Bar         Main    Satellite   Satellite
    ## 1311            Default       Soup         Main    Satellite   Satellite
    ## 1312            Default       Soup         Main    Satellite   Satellite
    ## 1313            Default  Grab N Go         Side    Satellite   Satellite
    ## 1314            Default Quesadilla         Main    Satellite   Satellite
    ## 1315            Default      Grill         Main    Treatment   Treatment
    ## 1316            Default  Grab N Go         Side    Satellite   Satellite
    ## 1317            Default Quesadilla         Main    Satellite   Satellite
    ## 1318            Default      Grill         Side    Treatment   Treatment
    ## 1319            Default      Grill         Main    Treatment   Treatment
    ## 1320            Default      Grill         Side    Treatment   Treatment
    ## 1321            Default      Grill         Main    Treatment   Treatment
    ## 1322            Default Quesadilla         Main    Satellite   Satellite
    ## 1323            Default      Grill         Main    Treatment   Treatment
    ## 1324            Default      Grill         Main    Treatment   Treatment
    ## 1325            Default      Grill Modification    Treatment   Treatment
    ## 1326            Default      Grill Modification    Treatment   Treatment
    ## 1327            Default      Grill Modification    Treatment   Treatment
    ## 1328            Default      Grill Modification    Treatment   Treatment
    ## 1329            Default      Grill Modification    Treatment   Treatment
    ## 1330            Default      Grill Modification    Treatment   Treatment
    ## 1331            Default        Wok         Main    Satellite   Satellite
    ## 1332            Default        Wok         Main    Satellite   Satellite
    ## 1333            Default      Ramen         Main    Treatment   Treatment
    ## 1334            Default        Wok         Main    Satellite   Satellite
    ## 1335            Default      Ramen         Main    Treatment   Treatment
    ## 1336            Default        Wok         Side    Satellite   Satellite
    ## 1337            Default        Wok         Main    Satellite   Satellite
    ## 1338            Default        Wok         Side    Satellite   Satellite
    ## 1339            Default        Wok         Side    Satellite   Satellite
    ## 1340            Default      Pasta         Main    Satellite   Satellite
    ## 1341            Default      Pizza         Main    Satellite   Satellite
    ## 1342            Default      Pasta         Main    Satellite   Satellite
    ## 1343            Default      Pizza         Main    Satellite   Satellite
    ## 1344            Default      Pasta Modification    Satellite   Satellite
    ## 1345            Default  Breakfast         Main    Satellite   Satellite
    ## 1346            Default  Breakfast         Side    Satellite   Satellite
    ## 1347            Default  Breakfast         Main    Satellite   Satellite
    ## 1348            Default  Breakfast         Main    Satellite   Satellite
    ## 1349            Default  Breakfast         Main    Satellite   Satellite
    ## 1350            Default  Breakfast Modification    Satellite   Satellite
    ## 1351            Default  Breakfast         Side    Satellite   Satellite
    ## 1352            Default  Breakfast         Side    Satellite   Satellite
    ## 1353            Default  Breakfast         Side    Satellite   Satellite
    ## 1354            Default  Breakfast         Side    Satellite   Satellite
    ## 1355            Default  Breakfast         Side    Satellite   Satellite
    ## 1356            Default       Wrap         Main    Satellite   Satellite
    ## 1357            Default       Wrap         Main    Satellite   Satellite
    ## 1358            Default       Wrap         Side    Satellite   Satellite
    ## 1359            Default       Wrap Modification    Satellite   Satellite
    ## 1360            Default       Soup         Main    Satellite   Satellite
    ## 1361            Default       Soup         Main    Satellite   Satellite
    ## 1362            Default  Salad Bar         Main    Satellite   Satellite
    ## 1363            Default  Salad Bar Modification    Satellite   Satellite
    ## 1364            Default  Grab N Go         Side    Satellite   Satellite
    ## 1365            Default Quesadilla         Main    Satellite   Satellite
    ## 1366            Default      Grill         Main    Treatment   Treatment
    ## 1367            Default  Grab N Go         Side    Satellite   Satellite
    ## 1368            Default Quesadilla         Main    Satellite   Satellite
    ## 1369            Default      Grill         Side    Treatment   Treatment
    ## 1370            Default      Grill         Main    Treatment   Treatment
    ## 1371            Default      Grill         Main    Treatment   Treatment
    ## 1372            Default Quesadilla         Main    Satellite   Satellite
    ## 1373            Default      Grill         Side    Treatment   Treatment
    ## 1374            Default      Grill Modification    Treatment   Treatment
    ## 1375            Default      Grill Modification    Treatment   Treatment
    ## 1376            Default      Grill         Main    Treatment   Treatment
    ## 1377            Default      Grill         Main    Treatment   Treatment
    ## 1378            Default      Grill Modification    Treatment   Treatment
    ## 1379            Default      Grill Modification    Treatment   Treatment
    ## 1380            Default        Wok         Main    Satellite   Satellite
    ## 1381            Default        Wok         Main    Satellite   Satellite
    ## 1382            Default      Ramen         Main    Treatment   Treatment
    ## 1383            Default        Wok         Main    Satellite   Satellite
    ## 1384            Default      Ramen         Main    Treatment   Treatment
    ## 1385            Default        Wok         Side    Satellite   Satellite
    ## 1386            Default        Wok         Side    Satellite   Satellite
    ## 1387            Default        Wok         Main    Satellite   Satellite
    ## 1388            Default        Wok         Side    Satellite   Satellite
    ## 1389            Default        Wok         Side    Satellite   Satellite
    ## 1390            Default        Wok         Side    Satellite   Satellite
    ## 1391            Default  Breakfast         Main    Satellite   Satellite
    ## 1392            Default  Breakfast         Side    Satellite   Satellite
    ## 1393            Default  Breakfast         Main    Satellite   Satellite
    ## 1394            Default  Breakfast         Main    Satellite   Satellite
    ## 1395            Default  Breakfast         Main    Satellite   Satellite
    ## 1396            Default  Breakfast         Side    Satellite   Satellite
    ## 1397            Default  Breakfast Modification    Satellite   Satellite
    ## 1398            Default  Breakfast         Side    Satellite   Satellite
    ## 1399            Default  Breakfast         Side    Satellite   Satellite
    ## 1400            Default  Breakfast         Side    Satellite   Satellite
    ## 1401            Default  Breakfast         Side    Satellite   Satellite
    ## 1402            Default  Breakfast         Side    Satellite   Satellite
    ## 1403            Default      Pasta         Main    Satellite   Satellite
    ## 1404            Default      Pizza         Main    Satellite   Satellite
    ## 1405            Default      Pizza         Main    Satellite   Satellite
    ## 1406            Default      Pasta         Main    Satellite   Satellite
    ## 1407            Default      Pasta Modification    Satellite   Satellite
    ## 1408            Default       Wrap         Main    Satellite   Satellite
    ## 1409            Default       Wrap         Main    Satellite   Satellite
    ## 1410            Default       Soup         Main    Satellite   Satellite
    ## 1411            Default       Soup         Main    Satellite   Satellite
    ## 1412            Default  Salad Bar         Main    Satellite   Satellite
    ## 1413            Default  Grab N Go         Side    Satellite   Satellite
    ## 1414         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1415         Multimodal      Grill         Main    Treatment   Treatment
    ## 1416         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1417         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1418         Multimodal      Grill         Side    Treatment   Treatment
    ## 1419         Multimodal      Grill         Main    Treatment   Treatment
    ## 1420         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1421         Multimodal      Grill         Main    Treatment   Treatment
    ## 1422         Multimodal      Grill         Side    Treatment   Treatment
    ## 1423         Multimodal      Grill         Main    Treatment   Treatment
    ## 1424         Multimodal      Grill Modification    Treatment   Treatment
    ## 1425         Multimodal      Grill Modification    Treatment   Treatment
    ## 1426         Multimodal      Grill         Main    Treatment   Treatment
    ## 1427         Multimodal      Grill Modification    Treatment   Treatment
    ## 1428         Multimodal      Grill Modification    Treatment   Treatment
    ## 1429         Multimodal      Grill Modification    Treatment   Treatment
    ## 1430         Multimodal      Grill Modification    Treatment   Treatment
    ## 1431         Multimodal      Grill Modification    Treatment   Treatment
    ## 1432         Multimodal        Wok         Main    Satellite   Satellite
    ## 1433         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1434         Multimodal        Wok         Main    Satellite   Satellite
    ## 1435         Multimodal        Wok         Main    Satellite   Satellite
    ## 1436         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1437         Multimodal        Wok         Side    Satellite   Satellite
    ## 1438         Multimodal        Wok         Side    Satellite   Satellite
    ## 1439         Multimodal        Wok         Side    Satellite   Satellite
    ## 1440         Multimodal        Wok         Main    Satellite   Satellite
    ## 1441         Multimodal        Wok         Side    Satellite   Satellite
    ## 1442         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1443         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1444         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1445         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1446         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1447         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1448         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1449         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1450         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1451         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1452         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1453         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1454         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1455         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1456         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1457         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1458         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1459         Multimodal       Wrap Modification    Satellite   Satellite
    ## 1460         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1461         Multimodal       Soup         Main    Satellite   Satellite
    ## 1462         Multimodal       Soup         Main    Satellite   Satellite
    ## 1463         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1464         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1465         Multimodal      Grill         Main    Treatment   Treatment
    ## 1466         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1467         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1468         Multimodal      Grill         Side    Treatment   Treatment
    ## 1469         Multimodal      Grill         Main    Treatment   Treatment
    ## 1470         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1471         Multimodal      Grill         Side    Treatment   Treatment
    ## 1472         Multimodal      Grill         Main    Treatment   Treatment
    ## 1473         Multimodal      Grill         Main    Treatment   Treatment
    ## 1474         Multimodal      Grill         Main    Treatment   Treatment
    ## 1475         Multimodal      Grill Modification    Treatment   Treatment
    ## 1476         Multimodal      Grill Modification    Treatment   Treatment
    ## 1477         Multimodal      Grill Modification    Treatment   Treatment
    ## 1478         Multimodal        Wok         Main    Satellite   Satellite
    ## 1479         Multimodal        Wok         Main    Satellite   Satellite
    ## 1480         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1481         Multimodal        Wok         Main    Satellite   Satellite
    ## 1482         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1483         Multimodal        Wok         Side    Satellite   Satellite
    ## 1484         Multimodal        Wok         Main    Satellite   Satellite
    ## 1485         Multimodal        Wok         Side    Satellite   Satellite
    ## 1486         Multimodal        Wok         Side    Satellite   Satellite
    ## 1487         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1488         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1489         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1490         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1491         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1492         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1493         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1494         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1495         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1496         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1497         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1498         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1499         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1500         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1501         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1502         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1503         Multimodal       Wrap Modification    Satellite   Satellite
    ## 1504         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1505         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 1506         Multimodal       Soup         Main    Satellite   Satellite
    ## 1507         Multimodal       Soup         Main    Satellite   Satellite
    ## 1508         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1509         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1510         Multimodal      Grill         Main    Treatment   Treatment
    ## 1511         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1512         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1513         Multimodal      Grill         Side    Treatment   Treatment
    ## 1514         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1515         Multimodal      Grill         Main    Treatment   Treatment
    ## 1516         Multimodal      Grill         Side    Treatment   Treatment
    ## 1517         Multimodal      Grill Modification    Treatment   Treatment
    ## 1518         Multimodal      Grill         Main    Treatment   Treatment
    ## 1519         Multimodal      Grill         Main    Treatment   Treatment
    ## 1520         Multimodal      Grill Modification    Treatment   Treatment
    ## 1521         Multimodal      Grill Modification    Treatment   Treatment
    ## 1522         Multimodal      Grill Modification    Treatment   Treatment
    ## 1523         Multimodal        Wok         Main    Satellite   Satellite
    ## 1524         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1525         Multimodal        Wok         Main    Satellite   Satellite
    ## 1526         Multimodal        Wok         Main    Satellite   Satellite
    ## 1527         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1528         Multimodal        Wok         Main    Satellite   Satellite
    ## 1529         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1530         Multimodal        Wok         Side    Satellite   Satellite
    ## 1531         Multimodal        Wok         Side    Satellite   Satellite
    ## 1532         Multimodal        Wok         Side    Satellite   Satellite
    ## 1533         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1534         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1535         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1536         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1537         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1538         Multimodal      Pasta         Side    Satellite   Satellite
    ## 1539         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1540         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1541         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1542         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1543         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1544         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1545         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1546         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1547         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1548         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1549         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1550         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1551         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1552         Multimodal       Wrap         Side    Satellite   Satellite
    ## 1553         Multimodal       Soup         Main    Satellite   Satellite
    ## 1554         Multimodal       Soup         Main    Satellite   Satellite
    ## 1555         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1556         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 1557         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1558         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1559         Multimodal      Grill         Main    Treatment   Treatment
    ## 1560         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1561         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1562         Multimodal      Grill         Side    Treatment   Treatment
    ## 1563         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1564         Multimodal      Grill         Main    Treatment   Treatment
    ## 1565         Multimodal      Grill         Main    Treatment   Treatment
    ## 1566         Multimodal      Grill         Side    Treatment   Treatment
    ## 1567         Multimodal      Grill         Main    Treatment   Treatment
    ## 1568         Multimodal      Grill Modification    Treatment   Treatment
    ## 1569         Multimodal      Grill         Main    Treatment   Treatment
    ## 1570         Multimodal      Grill Modification    Treatment   Treatment
    ## 1571         Multimodal      Grill Modification    Treatment   Treatment
    ## 1572         Multimodal      Grill Modification    Treatment   Treatment
    ## 1573         Multimodal      Grill Modification    Treatment   Treatment
    ## 1574         Multimodal        Wok         Main    Satellite   Satellite
    ## 1575         Multimodal        Wok         Main    Satellite   Satellite
    ## 1576         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1577         Multimodal        Wok         Main    Satellite   Satellite
    ## 1578         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1579         Multimodal        Wok         Side    Satellite   Satellite
    ## 1580         Multimodal        Wok         Side    Satellite   Satellite
    ## 1581         Multimodal        Wok         Side    Satellite   Satellite
    ## 1582         Multimodal        Wok         Side    Satellite   Satellite
    ## 1583         Multimodal        Wok         Side    Satellite   Satellite
    ## 1584         Multimodal        Wok         Side    Satellite   Satellite
    ## 1585         Multimodal      Ramen Modification    Treatment   Treatment
    ## 1586         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1587         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1588         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1589         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1590         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1591         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1592         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1593         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1594         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1595         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1596         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1597         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1598         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1599         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1600         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1601         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1602         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1603         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1604         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1605         Multimodal       Wrap         Side    Satellite   Satellite
    ## 1606         Multimodal       Wrap Modification    Satellite   Satellite
    ## 1607         Multimodal       Wrap         Side    Satellite   Satellite
    ## 1608         Multimodal       Soup         Main    Satellite   Satellite
    ## 1609         Multimodal       Soup         Main    Satellite   Satellite
    ## 1610         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1611         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 1612         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1613         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1614         Multimodal      Grill         Main    Treatment   Treatment
    ## 1615         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1616         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1617         Multimodal      Grill         Side    Treatment   Treatment
    ## 1618         Multimodal      Grill         Main    Treatment   Treatment
    ## 1619         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1620         Multimodal      Grill         Side    Treatment   Treatment
    ## 1621         Multimodal      Grill         Main    Treatment   Treatment
    ## 1622         Multimodal      Grill         Main    Treatment   Treatment
    ## 1623         Multimodal      Grill Modification    Treatment   Treatment
    ## 1624         Multimodal      Grill Modification    Treatment   Treatment
    ## 1625         Multimodal      Grill Modification    Treatment   Treatment
    ## 1626         Multimodal      Grill         Main    Treatment   Treatment
    ## 1627         Multimodal      Grill Modification    Treatment   Treatment
    ## 1628         Multimodal      Grill Modification    Treatment   Treatment
    ## 1629         Multimodal        Wok         Main    Satellite   Satellite
    ## 1630         Multimodal        Wok         Main    Satellite   Satellite
    ## 1631         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1632         Multimodal        Wok         Main    Satellite   Satellite
    ## 1633         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1634         Multimodal        Wok         Side    Satellite   Satellite
    ## 1635         Multimodal        Wok         Main    Satellite   Satellite
    ## 1636         Multimodal        Wok         Side    Satellite   Satellite
    ## 1637         Multimodal        Wok         Side    Satellite   Satellite
    ## 1638         Multimodal        Wok         Side    Satellite   Satellite
    ## 1639         Multimodal        Wok         Side    Satellite   Satellite
    ## 1640         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1641         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1642         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1643         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1644         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1645         Multimodal      Pasta         Side    Satellite   Satellite
    ## 1646         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1647         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1648         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1649         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1650         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1651         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1652         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1653         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1654         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1655         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1656         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1657         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1658         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1659         Multimodal       Wrap         Side    Satellite   Satellite
    ## 1660         Multimodal       Wrap Modification    Satellite   Satellite
    ## 1661         Multimodal       Soup         Main    Satellite   Satellite
    ## 1662         Multimodal       Soup         Main    Satellite   Satellite
    ## 1663         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1664         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 1665         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1666         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1667         Multimodal      Grill         Main    Treatment   Treatment
    ## 1668         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1669         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1670         Multimodal      Grill         Side    Treatment   Treatment
    ## 1671         Multimodal      Grill         Main    Treatment   Treatment
    ## 1672         Multimodal      Grill         Main    Treatment   Treatment
    ## 1673         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1674         Multimodal      Grill         Side    Treatment   Treatment
    ## 1675         Multimodal      Grill         Main    Treatment   Treatment
    ## 1676         Multimodal      Grill         Main    Treatment   Treatment
    ## 1677         Multimodal      Grill Modification    Treatment   Treatment
    ## 1678         Multimodal      Grill Modification    Treatment   Treatment
    ## 1679         Multimodal      Grill Modification    Treatment   Treatment
    ## 1680         Multimodal      Grill Modification    Treatment   Treatment
    ## 1681         Multimodal      Grill Modification    Treatment   Treatment
    ## 1682         Multimodal        Wok         Main    Satellite   Satellite
    ## 1683         Multimodal        Wok         Main    Satellite   Satellite
    ## 1684         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1685         Multimodal        Wok         Main    Satellite   Satellite
    ## 1686         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1687         Multimodal        Wok         Side    Satellite   Satellite
    ## 1688         Multimodal        Wok         Main    Satellite   Satellite
    ## 1689         Multimodal        Wok         Side    Satellite   Satellite
    ## 1690         Multimodal        Wok         Side    Satellite   Satellite
    ## 1691         Multimodal        Wok         Side    Satellite   Satellite
    ## 1692         Multimodal        Wok         Side    Satellite   Satellite
    ## 1693         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1694         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1695         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1696         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1697         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1698         Multimodal      Pasta         Side    Satellite   Satellite
    ## 1699         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1700         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1701         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1702         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1703         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1704         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1705         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1706         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1707         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1708         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1709         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1710         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1711         Multimodal       Wrap         Side    Satellite   Satellite
    ## 1712         Multimodal       Wrap Modification    Satellite   Satellite
    ## 1713         Multimodal       Soup         Main    Satellite   Satellite
    ## 1714         Multimodal       Soup         Main    Satellite   Satellite
    ## 1715         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1716         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 1717         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1718         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1719         Multimodal      Grill         Main    Treatment   Treatment
    ## 1720         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1721         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1722         Multimodal      Grill         Side    Treatment   Treatment
    ## 1723         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 1724         Multimodal      Grill         Main    Treatment   Treatment
    ## 1725         Multimodal      Grill         Main    Treatment   Treatment
    ## 1726         Multimodal      Grill         Main    Treatment   Treatment
    ## 1727         Multimodal      Grill         Main    Treatment   Treatment
    ## 1728         Multimodal      Grill         Side    Treatment   Treatment
    ## 1729         Multimodal      Grill Modification    Treatment   Treatment
    ## 1730         Multimodal      Grill Modification    Treatment   Treatment
    ## 1731         Multimodal      Grill Modification    Treatment   Treatment
    ## 1732         Multimodal      Grill Modification    Treatment   Treatment
    ## 1733         Multimodal        Wok         Main    Satellite   Satellite
    ## 1734         Multimodal        Wok         Main    Satellite   Satellite
    ## 1735         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1736         Multimodal        Wok         Main    Satellite   Satellite
    ## 1737         Multimodal      Ramen         Main    Treatment   Treatment
    ## 1738         Multimodal        Wok         Main    Satellite   Satellite
    ## 1739         Multimodal        Wok         Side    Satellite   Satellite
    ## 1740         Multimodal        Wok         Side    Satellite   Satellite
    ## 1741         Multimodal        Wok         Side    Satellite   Satellite
    ## 1742         Multimodal        Wok         Side    Satellite   Satellite
    ## 1743         Multimodal        Wok         Side    Satellite   Satellite
    ## 1744         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1745         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1746         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1747         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1748         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 1749         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 1750         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1751         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1752         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1753         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1754         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1755         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 1756         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1757         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1758         Multimodal      Pasta         Main    Satellite   Satellite
    ## 1759         Multimodal      Pizza         Main    Satellite   Satellite
    ## 1760         Multimodal      Pasta Modification    Satellite   Satellite
    ## 1761         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1762         Multimodal       Wrap         Main    Satellite   Satellite
    ## 1763         Multimodal       Soup         Main    Satellite   Satellite
    ## 1764         Multimodal       Soup         Main    Satellite   Satellite
    ## 1765         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 1766         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 1767 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1768 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1769 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1770 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1771 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1772 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1773 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1774 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1775 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1776 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1777 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1778 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1779 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1780 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1781 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1782 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1783 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1784 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1785 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1786 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1787 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1788 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1789 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1790 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1791 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1792 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1793 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1794 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1795 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1796 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 1797 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1798 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1799 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1800 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1801 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1802 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 1803 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1804 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1805 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1806 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1807 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1808 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1809 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1810 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1811 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1812 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 1813 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1814 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1815 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1816 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1817 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1818 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1819 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1820 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1821 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1822 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1823 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1824 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1825 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1826 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1827 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1828 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1829 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1830 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1831 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1832 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1833 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1834 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1835 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1836 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1837 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1838 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1839 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1840 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1841 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 1842 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1843 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1844 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1845 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 1846 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1847 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1848 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1849 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1850 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1851 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1852 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 1853 Multimodal (Extra)       Wrap Modification    Satellite   Satellite
    ## 1854 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 1855 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1856 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1857 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1858 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1859 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1860 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1861 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1862 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1863 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1864 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1865 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1866 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1867 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1868 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1869 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1870 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1871 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1872 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1873 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1874 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1875 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1876 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1877 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1878 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1879 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1880 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1881 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1882 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1883 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1884 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1885 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1886 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1887 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1888 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 1889 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1890 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1891 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1892 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1893 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 1894 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1895 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1896 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1897 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1898 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1899 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1900 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 1901 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 1902 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1903 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1904 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1905 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1906 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1907 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1908 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1909 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1910 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1911 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1912 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1913 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1914 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1915 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1916 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1917 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1918 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1919 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1920 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1921 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1922 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1923 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1924 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1925 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1926 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1927 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1928 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1929 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1930 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1931 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1932 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1933 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1934 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1935 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1936 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1937 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 1938 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1939 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1940 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1941 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1942 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1943 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1944 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1945 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1946 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1947 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1948 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 1949 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1950 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1951 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 1952 Multimodal (Extra)       Wrap Modification    Satellite   Satellite
    ## 1953 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 1954 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1955 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 1956 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1957 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1958 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1959 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1960 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 1961 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 1962 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1963 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 1964 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1965 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1966 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1967 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1968 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 1969 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1970 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1971 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 1972 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1973 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1974 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1975 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1976 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 1977 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1978 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1979 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1980 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 1981 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1982 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 1983 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1984 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1985 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 1986 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 1987 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1988 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1989 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1990 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 1991 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1992 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1993 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 1994 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 1995 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 1996 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1997 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 1998 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 1999 Multimodal (Extra)       Wrap Modification    Satellite   Satellite
    ## 2000 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2001 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2002 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2003 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2004 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2005 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2006 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2007 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2008 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2009 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2010 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2011 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2012 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2013 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2014 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2015 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2016 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2017 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2018 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2019 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2020 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2021 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2022 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2023 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2024 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2025 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2026 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2027 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2028 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2029 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2030 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2031 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2032 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2033 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2034 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 2035 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2036 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2037 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2038 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2039 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2040 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2041 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2042 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 2043 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2044 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2045 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2046 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2047 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2048 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2049 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2050 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2051 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2052 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2053 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2054 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2055 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2056 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2057 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2058 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2059 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2060 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2061 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2062 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2063 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2064 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2065 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2066 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2067 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2068 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2069 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2070 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2071 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2072 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2073 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2074 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2075 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 2076 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2077 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2078 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2079 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2080 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2081 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2082 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 2083 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2084 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 2085 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2086 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2087 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2088 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2089 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2090 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2091 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2092 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2093 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2094 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2095 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2096 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2097 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2098 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2099 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2100 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2101 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2102 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2103 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2104 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2105 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2106 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2107 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2108 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2109 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2110 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2111 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2112 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2113 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2114 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2115 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2116 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2117 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 2118 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2119 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2120 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2121 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2122 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2123 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2124 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2125 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2126 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2127 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 2128 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2129 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2130 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2131 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2132 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2133 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2134 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2135 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2136 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2137 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2138 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2139 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2140 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2141 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2142 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2143 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2144 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2145 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2146 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2147 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2148 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2149 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2150 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2151 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2152 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2153 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2154 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2155 Multimodal (Extra)        Wok         Side    Satellite   Satellite
    ## 2156 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2157 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2158 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2159 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 2160 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2161 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2162 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2163 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2164 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2165 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2166 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2167 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2168 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2169 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 2170 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2171 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2172 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2173 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2174 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2175 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2176 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2177 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2178 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2179 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2180 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2181 Multimodal (Extra) Quesadilla         Main    Satellite   Satellite
    ## 2182 Multimodal (Extra)      Grill         Side    Treatment   Treatment
    ## 2183 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2184 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2185 Multimodal (Extra)      Grill         Main    Treatment   Treatment
    ## 2186 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2187 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2188 Multimodal (Extra)      Grill Modification    Treatment   Treatment
    ## 2189 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2190 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2191 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2192 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2193 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2194 Multimodal (Extra)      Ramen         Main    Treatment   Treatment
    ## 2195 Multimodal (Extra)        Wok         Main    Satellite   Satellite
    ## 2196 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2197 Multimodal (Extra)  Breakfast         Main    Satellite   Satellite
    ## 2198 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2199 Multimodal (Extra)  Breakfast Modification    Satellite   Satellite
    ## 2200 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2201 Multimodal (Extra)  Breakfast         Side    Satellite   Satellite
    ## 2202 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2203 Multimodal (Extra)      Pasta         Main    Satellite   Satellite
    ## 2204 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2205 Multimodal (Extra)      Pizza         Main    Satellite   Satellite
    ## 2206 Multimodal (Extra)      Pasta Modification    Satellite   Satellite
    ## 2207 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2208 Multimodal (Extra)       Wrap         Main    Satellite   Satellite
    ## 2209 Multimodal (Extra)       Wrap         Side    Satellite   Satellite
    ## 2210 Multimodal (Extra)       Wrap Modification    Satellite   Satellite
    ## 2211 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2212 Multimodal (Extra)       Soup         Main    Satellite   Satellite
    ## 2213 Multimodal (Extra)  Salad Bar         Main    Satellite   Satellite
    ## 2214 Multimodal (Extra)  Grab N Go         Side    Satellite   Satellite
    ## 2215            Control Quesadilla         Main    Satellite   Satellite
    ## 2216            Control      Grill         Main    Treatment   Treatment
    ## 2217            Control  Grab N Go         Side    Satellite   Satellite
    ## 2218            Control Quesadilla         Main    Satellite   Satellite
    ## 2219            Control      Grill         Side    Treatment   Treatment
    ## 2220            Control      Grill         Main    Treatment   Treatment
    ## 2221            Control Quesadilla         Main    Satellite   Satellite
    ## 2222            Control      Grill         Side    Treatment   Treatment
    ## 2223            Control      Grill         Main    Treatment   Treatment
    ## 2224            Control      Grill Modification    Treatment   Treatment
    ## 2225            Control      Grill         Main    Treatment   Treatment
    ## 2226            Control  Grab N Go         Side    Satellite   Satellite
    ## 2227            Control      Grill Modification    Treatment   Treatment
    ## 2228            Control      Grill         Main    Treatment   Treatment
    ## 2229            Control      Grill Modification    Treatment   Treatment
    ## 2230            Control      Grill Modification    Treatment   Treatment
    ## 2231            Control      Grill Modification    Treatment   Treatment
    ## 2232            Control        Wok         Main    Satellite   Satellite
    ## 2233            Control      Ramen         Main    Treatment   Treatment
    ## 2234            Control        Wok         Main    Satellite   Satellite
    ## 2235            Control        Wok         Main    Satellite   Satellite
    ## 2236            Control      Ramen         Main    Treatment   Treatment
    ## 2237            Control        Wok         Main    Satellite   Satellite
    ## 2238            Control        Wok         Side    Satellite   Satellite
    ## 2239            Control        Wok         Side    Satellite   Satellite
    ## 2240            Control        Wok         Side    Satellite   Satellite
    ## 2241            Control        Wok         Side    Satellite   Satellite
    ## 2242            Control        Wok         Side    Satellite   Satellite
    ## 2243            Control      Pasta         Main    Satellite   Satellite
    ## 2244            Control      Pasta         Main    Satellite   Satellite
    ## 2245            Control      Pizza         Main    Satellite   Satellite
    ## 2246            Control      Pizza         Main    Satellite   Satellite
    ## 2247            Control      Pasta Modification    Satellite   Satellite
    ## 2248            Control  Breakfast         Main    Satellite   Satellite
    ## 2249            Control  Breakfast         Side    Satellite   Satellite
    ## 2250            Control  Breakfast         Main    Satellite   Satellite
    ## 2251            Control  Breakfast         Main    Satellite   Satellite
    ## 2252            Control  Breakfast         Main    Satellite   Satellite
    ## 2253            Control  Breakfast Modification    Satellite   Satellite
    ## 2254            Control  Breakfast         Side    Satellite   Satellite
    ## 2255            Control  Breakfast         Side    Satellite   Satellite
    ## 2256            Control  Breakfast         Side    Satellite   Satellite
    ## 2257            Control  Breakfast         Side    Satellite   Satellite
    ## 2258            Control  Breakfast         Side    Satellite   Satellite
    ## 2259            Control  Breakfast         Side    Satellite   Satellite
    ## 2260            Control       Wrap         Main    Satellite   Satellite
    ## 2261            Control       Wrap         Main    Satellite   Satellite
    ## 2262            Control  Salad Bar         Main    Satellite   Satellite
    ## 2263            Control       Soup         Main    Satellite   Satellite
    ## 2264            Control       Soup         Main    Satellite   Satellite
    ## 2265            Control Quesadilla         Main    Satellite   Satellite
    ## 2266            Control      Grill         Main    Treatment   Treatment
    ## 2267            Control  Grab N Go         Side    Satellite   Satellite
    ## 2268            Control Quesadilla         Main    Satellite   Satellite
    ## 2269            Control      Grill         Side    Treatment   Treatment
    ## 2270            Control      Grill         Main    Treatment   Treatment
    ## 2271            Control      Grill         Main    Treatment   Treatment
    ## 2272            Control Quesadilla         Main    Satellite   Satellite
    ## 2273            Control      Grill         Side    Treatment   Treatment
    ## 2274            Control      Grill Modification    Treatment   Treatment
    ## 2275            Control      Grill         Main    Treatment   Treatment
    ## 2276            Control  Grab N Go         Side    Satellite   Satellite
    ## 2277            Control      Grill         Main    Treatment   Treatment
    ## 2278            Control      Grill Modification    Treatment   Treatment
    ## 2279            Control      Grill Modification    Treatment   Treatment
    ## 2280            Control      Grill Modification    Treatment   Treatment
    ## 2281            Control      Grill Modification    Treatment   Treatment
    ## 2282            Control        Wok         Main    Satellite   Satellite
    ## 2283            Control      Ramen         Main    Treatment   Treatment
    ## 2284            Control        Wok         Main    Satellite   Satellite
    ## 2285            Control        Wok         Main    Satellite   Satellite
    ## 2286            Control      Ramen         Main    Treatment   Treatment
    ## 2287            Control        Wok         Main    Satellite   Satellite
    ## 2288            Control        Wok         Side    Satellite   Satellite
    ## 2289            Control        Wok         Side    Satellite   Satellite
    ## 2290            Control        Wok         Side    Satellite   Satellite
    ## 2291            Control        Wok         Side    Satellite   Satellite
    ## 2292            Control        Wok         Side    Satellite   Satellite
    ## 2293            Control      Pasta         Main    Satellite   Satellite
    ## 2294            Control      Pasta         Main    Satellite   Satellite
    ## 2295            Control      Pizza         Main    Satellite   Satellite
    ## 2296            Control      Pasta Modification    Satellite   Satellite
    ## 2297            Control      Pizza         Main    Satellite   Satellite
    ## 2298            Control  Breakfast         Main    Satellite   Satellite
    ## 2299            Control  Breakfast         Side    Satellite   Satellite
    ## 2300            Control  Breakfast         Main    Satellite   Satellite
    ## 2301            Control  Breakfast Modification    Satellite   Satellite
    ## 2302            Control  Breakfast         Side    Satellite   Satellite
    ## 2303            Control  Breakfast         Side    Satellite   Satellite
    ## 2304            Control  Breakfast         Side    Satellite   Satellite
    ## 2305            Control  Breakfast         Side    Satellite   Satellite
    ## 2306            Control  Breakfast         Side    Satellite   Satellite
    ## 2307            Control       Wrap         Main    Satellite   Satellite
    ## 2308            Control       Wrap         Side    Satellite   Satellite
    ## 2309            Control  Salad Bar         Main    Satellite   Satellite
    ## 2310            Control  Salad Bar Modification    Satellite   Satellite
    ## 2311            Control       Soup         Main    Satellite   Satellite
    ## 2312            Control       Soup         Main    Satellite   Satellite
    ## 2313            Control  Grab N Go         Main    Satellite   Satellite
    ## 2314            Control  Grab N Go         Main    Satellite   Satellite
    ## 2315            Control Quesadilla         Main    Satellite   Satellite
    ## 2316            Control      Grill         Main    Treatment   Treatment
    ## 2317            Control Quesadilla         Main    Satellite   Satellite
    ## 2318            Control  Grab N Go         Side    Satellite   Satellite
    ## 2319            Control      Grill         Side    Treatment   Treatment
    ## 2320            Control      Grill         Main    Treatment   Treatment
    ## 2321            Control Quesadilla         Main    Satellite   Satellite
    ## 2322            Control      Grill         Main    Treatment   Treatment
    ## 2323            Control      Grill         Main    Treatment   Treatment
    ## 2324            Control      Grill         Side    Treatment   Treatment
    ## 2325            Control      Grill Modification    Treatment   Treatment
    ## 2326            Control  Grab N Go         Side    Satellite   Satellite
    ## 2327            Control      Grill Modification    Treatment   Treatment
    ## 2328            Control      Grill         Main    Treatment   Treatment
    ## 2329            Control      Grill Modification    Treatment   Treatment
    ## 2330            Control      Grill Modification    Treatment   Treatment
    ## 2331            Control      Grill Modification    Treatment   Treatment
    ## 2332            Control        Wok         Main    Satellite   Satellite
    ## 2333            Control      Ramen         Main    Treatment   Treatment
    ## 2334            Control        Wok         Main    Satellite   Satellite
    ## 2335            Control        Wok         Main    Satellite   Satellite
    ## 2336            Control      Ramen         Main    Treatment   Treatment
    ## 2337            Control        Wok         Side    Satellite   Satellite
    ## 2338            Control        Wok         Main    Satellite   Satellite
    ## 2339            Control        Wok         Side    Satellite   Satellite
    ## 2340            Control        Wok         Side    Satellite   Satellite
    ## 2341            Control        Wok         Side    Satellite   Satellite
    ## 2342            Control      Pasta         Main    Satellite   Satellite
    ## 2343            Control      Pasta         Main    Satellite   Satellite
    ## 2344            Control      Pizza         Main    Satellite   Satellite
    ## 2345            Control      Pizza         Main    Satellite   Satellite
    ## 2346            Control      Pasta Modification    Satellite   Satellite
    ## 2347            Control      Pasta         Side    Satellite   Satellite
    ## 2348            Control  Breakfast         Main    Satellite   Satellite
    ## 2349            Control  Breakfast         Side    Satellite   Satellite
    ## 2350            Control  Breakfast         Main    Satellite   Satellite
    ## 2351            Control  Breakfast Modification    Satellite   Satellite
    ## 2352            Control  Breakfast         Side    Satellite   Satellite
    ## 2353            Control  Breakfast         Side    Satellite   Satellite
    ## 2354            Control  Breakfast         Side    Satellite   Satellite
    ## 2355            Control  Breakfast         Side    Satellite   Satellite
    ## 2356            Control  Breakfast         Side    Satellite   Satellite
    ## 2357            Control       Wrap         Main    Satellite   Satellite
    ## 2358            Control       Wrap         Main    Satellite   Satellite
    ## 2359            Control       Wrap         Side    Satellite   Satellite
    ## 2360            Control       Wrap Modification    Satellite   Satellite
    ## 2361            Control       Wrap         Side    Satellite   Satellite
    ## 2362            Control  Salad Bar         Main    Satellite   Satellite
    ## 2363            Control  Salad Bar Modification    Satellite   Satellite
    ## 2364            Control       Soup         Main    Satellite   Satellite
    ## 2365            Control       Soup         Main    Satellite   Satellite
    ## 2366            Control  Grab N Go         Main    Satellite   Satellite
    ## 2367            Control  Grab N Go         Main    Satellite   Satellite
    ## 2368            Control Quesadilla         Main    Satellite   Satellite
    ## 2369            Control  Grab N Go         Side    Satellite   Satellite
    ## 2370            Control      Grill         Main    Treatment   Treatment
    ## 2371            Control Quesadilla         Main    Satellite   Satellite
    ## 2372            Control      Grill         Side    Treatment   Treatment
    ## 2373            Control      Grill         Main    Treatment   Treatment
    ## 2374            Control      Grill         Main    Treatment   Treatment
    ## 2375            Control Quesadilla         Main    Satellite   Satellite
    ## 2376            Control      Grill         Side    Treatment   Treatment
    ## 2377            Control      Grill Modification    Treatment   Treatment
    ## 2378            Control      Grill         Main    Treatment   Treatment
    ## 2379            Control  Grab N Go         Side    Satellite   Satellite
    ## 2380            Control      Grill Modification    Treatment   Treatment
    ## 2381            Control      Grill         Main    Treatment   Treatment
    ## 2382            Control      Grill Modification    Treatment   Treatment
    ## 2383            Control      Grill Modification    Treatment   Treatment
    ## 2384            Control      Grill Modification    Treatment   Treatment
    ## 2385            Control        Wok         Main    Satellite   Satellite
    ## 2386            Control        Wok         Main    Satellite   Satellite
    ## 2387            Control      Ramen         Main    Treatment   Treatment
    ## 2388            Control        Wok         Main    Satellite   Satellite
    ## 2389            Control      Ramen         Main    Treatment   Treatment
    ## 2390            Control        Wok         Side    Satellite   Satellite
    ## 2391            Control        Wok         Main    Satellite   Satellite
    ## 2392            Control        Wok         Side    Satellite   Satellite
    ## 2393            Control        Wok         Side    Satellite   Satellite
    ## 2394            Control        Wok         Side    Satellite   Satellite
    ## 2395            Control      Pasta         Main    Satellite   Satellite
    ## 2396            Control      Pizza         Main    Satellite   Satellite
    ## 2397            Control      Pasta         Main    Satellite   Satellite
    ## 2398            Control      Pizza         Main    Satellite   Satellite
    ## 2399            Control      Pasta Modification    Satellite   Satellite
    ## 2400            Control  Breakfast         Main    Satellite   Satellite
    ## 2401            Control  Breakfast         Side    Satellite   Satellite
    ## 2402            Control  Breakfast         Main    Satellite   Satellite
    ## 2403            Control  Breakfast Modification    Satellite   Satellite
    ## 2404            Control  Breakfast         Side    Satellite   Satellite
    ## 2405            Control  Breakfast         Side    Satellite   Satellite
    ## 2406            Control  Breakfast         Side    Satellite   Satellite
    ## 2407            Control       Wrap         Main    Satellite   Satellite
    ## 2408            Control       Wrap         Side    Satellite   Satellite
    ## 2409            Control       Wrap         Side    Satellite   Satellite
    ## 2410            Control       Wrap Modification    Satellite   Satellite
    ## 2411            Control       Soup         Main    Satellite   Satellite
    ## 2412            Control       Soup         Main    Satellite   Satellite
    ## 2413            Control  Salad Bar         Main    Satellite   Satellite
    ## 2414            Control  Salad Bar Modification    Satellite   Satellite
    ## 2415            Control  Grab N Go         Main    Satellite   Satellite
    ## 2416            Control  Grab N Go         Main    Satellite   Satellite
    ## 2417            Control Quesadilla         Main    Satellite   Satellite
    ## 2418            Control      Grill         Main    Treatment   Treatment
    ## 2419            Control  Grab N Go         Side    Satellite   Satellite
    ## 2420            Control Quesadilla         Main    Satellite   Satellite
    ## 2421            Control      Grill         Side    Treatment   Treatment
    ## 2422            Control      Grill         Main    Treatment   Treatment
    ## 2423            Control      Grill         Main    Treatment   Treatment
    ## 2424            Control      Grill         Side    Treatment   Treatment
    ## 2425            Control      Grill         Main    Treatment   Treatment
    ## 2426            Control      Grill Modification    Treatment   Treatment
    ## 2427            Control Quesadilla         Main    Satellite   Satellite
    ## 2428            Control  Grab N Go         Side    Satellite   Satellite
    ## 2429            Control      Grill         Main    Treatment   Treatment
    ## 2430            Control      Grill Modification    Treatment   Treatment
    ## 2431            Control      Grill Modification    Treatment   Treatment
    ## 2432            Control      Grill Modification    Treatment   Treatment
    ## 2433            Control      Grill Modification    Treatment   Treatment
    ## 2434            Control        Wok         Main    Satellite   Satellite
    ## 2435            Control      Ramen         Main    Treatment   Treatment
    ## 2436            Control        Wok         Main    Satellite   Satellite
    ## 2437            Control        Wok         Main    Satellite   Satellite
    ## 2438            Control      Ramen         Main    Treatment   Treatment
    ## 2439            Control        Wok         Side    Satellite   Satellite
    ## 2440            Control        Wok         Main    Satellite   Satellite
    ## 2441            Control        Wok         Side    Satellite   Satellite
    ## 2442            Control        Wok         Side    Satellite   Satellite
    ## 2443            Control        Wok         Side    Satellite   Satellite
    ## 2444            Control      Ramen         Main    Treatment   Treatment
    ## 2445            Control      Pasta         Main    Satellite   Satellite
    ## 2446            Control      Pasta         Main    Satellite   Satellite
    ## 2447            Control      Pizza         Main    Satellite   Satellite
    ## 2448            Control      Pasta Modification    Satellite   Satellite
    ## 2449            Control      Pizza         Main    Satellite   Satellite
    ## 2450            Control      Pasta         Side    Satellite   Satellite
    ## 2451            Control  Breakfast         Main    Satellite   Satellite
    ## 2452            Control  Breakfast         Side    Satellite   Satellite
    ## 2453            Control  Breakfast         Main    Satellite   Satellite
    ## 2454            Control  Breakfast Modification    Satellite   Satellite
    ## 2455            Control  Breakfast         Side    Satellite   Satellite
    ## 2456            Control  Breakfast         Side    Satellite   Satellite
    ## 2457            Control  Breakfast         Side    Satellite   Satellite
    ## 2458            Control       Wrap         Main    Satellite   Satellite
    ## 2459            Control       Wrap         Main    Satellite   Satellite
    ## 2460            Control       Wrap         Side    Satellite   Satellite
    ## 2461            Control  Salad Bar         Main    Satellite   Satellite
    ## 2462            Control  Salad Bar Modification    Satellite   Satellite
    ## 2463            Control       Soup         Main    Satellite   Satellite
    ## 2464            Control       Soup         Main    Satellite   Satellite
    ## 2465            Control  Grab N Go         Main    Satellite   Satellite
    ## 2466            Control  Grab N Go         Main    Satellite   Satellite
    ## 2467            Control Quesadilla         Main    Satellite   Satellite
    ## 2468            Control      Grill         Main    Treatment   Treatment
    ## 2469            Control  Grab N Go         Side    Satellite   Satellite
    ## 2470            Control Quesadilla         Main    Satellite   Satellite
    ## 2471            Control      Grill         Side    Treatment   Treatment
    ## 2472            Control      Grill         Main    Treatment   Treatment
    ## 2473            Control      Grill         Main    Treatment   Treatment
    ## 2474            Control      Grill         Side    Treatment   Treatment
    ## 2475            Control      Grill Modification    Treatment   Treatment
    ## 2476            Control Quesadilla         Main    Satellite   Satellite
    ## 2477            Control      Grill Modification    Treatment   Treatment
    ## 2478            Control      Grill         Main    Treatment   Treatment
    ## 2479            Control  Grab N Go         Side    Satellite   Satellite
    ## 2480            Control      Grill         Main    Treatment   Treatment
    ## 2481            Control      Grill Modification    Treatment   Treatment
    ## 2482            Control      Grill Modification    Treatment   Treatment
    ## 2483            Control      Grill Modification    Treatment   Treatment
    ## 2484            Control        Wok         Main    Satellite   Satellite
    ## 2485            Control      Ramen         Main    Treatment   Treatment
    ## 2486            Control        Wok         Main    Satellite   Satellite
    ## 2487            Control        Wok         Main    Satellite   Satellite
    ## 2488            Control      Ramen         Main    Treatment   Treatment
    ## 2489            Control        Wok         Side    Satellite   Satellite
    ## 2490            Control        Wok         Side    Satellite   Satellite
    ## 2491            Control        Wok         Main    Satellite   Satellite
    ## 2492            Control        Wok         Side    Satellite   Satellite
    ## 2493            Control        Wok         Side    Satellite   Satellite
    ## 2494            Control        Wok         Side    Satellite   Satellite
    ## 2495            Control      Pasta         Main    Satellite   Satellite
    ## 2496            Control      Pasta         Main    Satellite   Satellite
    ## 2497            Control      Pizza         Main    Satellite   Satellite
    ## 2498            Control      Pizza         Main    Satellite   Satellite
    ## 2499            Control      Pasta Modification    Satellite   Satellite
    ## 2500            Control      Pasta         Side    Satellite   Satellite
    ## 2501            Control  Breakfast         Main    Satellite   Satellite
    ## 2502            Control  Breakfast         Side    Satellite   Satellite
    ## 2503            Control  Breakfast         Main    Satellite   Satellite
    ## 2504            Control  Breakfast Modification    Satellite   Satellite
    ## 2505            Control  Breakfast         Side    Satellite   Satellite
    ## 2506            Control  Breakfast         Side    Satellite   Satellite
    ## 2507            Control  Breakfast         Side    Satellite   Satellite
    ## 2508            Control  Breakfast         Side    Satellite   Satellite
    ## 2509            Control       Wrap         Main    Satellite   Satellite
    ## 2510            Control       Wrap         Main    Satellite   Satellite
    ## 2511            Control       Wrap         Side    Satellite   Satellite
    ## 2512            Control       Wrap Modification    Satellite   Satellite
    ## 2513            Control       Soup         Main    Satellite   Satellite
    ## 2514            Control       Soup         Main    Satellite   Satellite
    ## 2515            Control  Salad Bar         Main    Satellite   Satellite
    ## 2516            Control  Salad Bar Modification    Satellite   Satellite
    ## 2517            Control  Grab N Go         Main    Satellite   Satellite
    ## 2518            Control  Grab N Go         Main    Satellite   Satellite
    ## 2519            Control Quesadilla         Main    Satellite   Satellite
    ## 2520            Control      Grill         Main    Treatment   Treatment
    ## 2521            Control  Grab N Go         Side    Satellite   Satellite
    ## 2522            Control Quesadilla         Main    Satellite   Satellite
    ## 2523            Control      Grill         Side    Treatment   Treatment
    ## 2524            Control Quesadilla         Main    Satellite   Satellite
    ## 2525            Control      Grill         Main    Treatment   Treatment
    ## 2526            Control      Grill         Main    Treatment   Treatment
    ## 2527            Control      Grill         Side    Treatment   Treatment
    ## 2528            Control      Grill         Main    Treatment   Treatment
    ## 2529            Control  Grab N Go         Side    Satellite   Satellite
    ## 2530            Control      Grill Modification    Treatment   Treatment
    ## 2531            Control      Grill         Main    Treatment   Treatment
    ## 2532            Control      Grill Modification    Treatment   Treatment
    ## 2533            Control      Grill Modification    Treatment   Treatment
    ## 2534            Control      Grill Modification    Treatment   Treatment
    ## 2535            Control      Grill Modification    Treatment   Treatment
    ## 2536            Control      Grill Modification    Treatment   Treatment
    ## 2537            Control      Grill Modification    Treatment   Treatment
    ## 2538            Control        Wok         Main    Satellite   Satellite
    ## 2539            Control        Wok         Main    Satellite   Satellite
    ## 2540            Control      Ramen         Main    Treatment   Treatment
    ## 2541            Control        Wok         Main    Satellite   Satellite
    ## 2542            Control      Ramen         Main    Treatment   Treatment
    ## 2543            Control        Wok         Side    Satellite   Satellite
    ## 2544            Control        Wok         Main    Satellite   Satellite
    ## 2545            Control        Wok         Side    Satellite   Satellite
    ## 2546            Control        Wok         Side    Satellite   Satellite
    ## 2547            Control      Pasta         Main    Satellite   Satellite
    ## 2548            Control      Pizza         Main    Satellite   Satellite
    ## 2549            Control      Pasta         Main    Satellite   Satellite
    ## 2550            Control      Pizza         Main    Satellite   Satellite
    ## 2551            Control      Pasta Modification    Satellite   Satellite
    ## 2552            Control  Breakfast         Main    Satellite   Satellite
    ## 2553            Control  Breakfast         Side    Satellite   Satellite
    ## 2554            Control  Breakfast         Main    Satellite   Satellite
    ## 2555            Control  Breakfast Modification    Satellite   Satellite
    ## 2556            Control  Breakfast         Side    Satellite   Satellite
    ## 2557            Control  Breakfast         Side    Satellite   Satellite
    ## 2558            Control  Breakfast         Side    Satellite   Satellite
    ## 2559            Control  Breakfast         Side    Satellite   Satellite
    ## 2560            Control  Breakfast         Side    Satellite   Satellite
    ## 2561            Control       Wrap         Main    Satellite   Satellite
    ## 2562            Control       Wrap         Main    Satellite   Satellite
    ## 2563            Control       Wrap Modification    Satellite   Satellite
    ## 2564            Control       Soup         Main    Satellite   Satellite
    ## 2565            Control       Soup         Main    Satellite   Satellite
    ## 2566            Control  Salad Bar         Main    Satellite   Satellite
    ## 2567            Control  Grab N Go         Main    Satellite   Satellite
    ## 2568            Control  Grab N Go         Main    Satellite   Satellite
    ## 2569            Control Quesadilla         Main    Satellite   Satellite
    ## 2570            Control      Grill         Main    Treatment   Treatment
    ## 2571            Control Quesadilla         Main    Satellite   Satellite
    ## 2572            Control  Grab N Go         Side    Satellite   Satellite
    ## 2573            Control      Grill         Side    Treatment   Treatment
    ## 2574            Control      Grill         Side    Treatment   Treatment
    ## 2575            Control Quesadilla         Main    Satellite   Satellite
    ## 2576            Control      Grill         Main    Treatment   Treatment
    ## 2577            Control      Grill         Main    Treatment   Treatment
    ## 2578            Control      Grill         Main    Treatment   Treatment
    ## 2579            Control      Grill Modification    Treatment   Treatment
    ## 2580            Control  Grab N Go         Side    Satellite   Satellite
    ## 2581            Control      Grill         Main    Treatment   Treatment
    ## 2582            Control      Grill Modification    Treatment   Treatment
    ## 2583            Control      Grill Modification    Treatment   Treatment
    ## 2584            Control      Grill Modification    Treatment   Treatment
    ## 2585            Control      Grill Modification    Treatment   Treatment
    ## 2586            Control      Grill Modification    Treatment   Treatment
    ## 2587            Control        Wok         Main    Satellite   Satellite
    ## 2588            Control        Wok         Main    Satellite   Satellite
    ## 2589            Control      Ramen         Main    Treatment   Treatment
    ## 2590            Control        Wok         Main    Satellite   Satellite
    ## 2591            Control      Ramen         Main    Treatment   Treatment
    ## 2592            Control        Wok         Side    Satellite   Satellite
    ## 2593            Control        Wok         Side    Satellite   Satellite
    ## 2594            Control        Wok         Main    Satellite   Satellite
    ## 2595            Control        Wok         Side    Satellite   Satellite
    ## 2596            Control        Wok         Side    Satellite   Satellite
    ## 2597            Control        Wok         Side    Satellite   Satellite
    ## 2598            Control      Pasta         Main    Satellite   Satellite
    ## 2599            Control      Pasta         Main    Satellite   Satellite
    ## 2600            Control      Pizza         Main    Satellite   Satellite
    ## 2601            Control      Pizza         Main    Satellite   Satellite
    ## 2602            Control      Pasta Modification    Satellite   Satellite
    ## 2603            Control  Breakfast         Main    Satellite   Satellite
    ## 2604            Control  Breakfast         Side    Satellite   Satellite
    ## 2605            Control  Breakfast         Main    Satellite   Satellite
    ## 2606            Control  Breakfast Modification    Satellite   Satellite
    ## 2607            Control  Breakfast         Side    Satellite   Satellite
    ## 2608            Control  Breakfast         Side    Satellite   Satellite
    ## 2609            Control  Breakfast         Side    Satellite   Satellite
    ## 2610            Control  Breakfast         Side    Satellite   Satellite
    ## 2611            Control       Wrap         Main    Satellite   Satellite
    ## 2612            Control       Wrap         Main    Satellite   Satellite
    ## 2613            Control       Wrap         Side    Satellite   Satellite
    ## 2614            Control       Wrap Modification    Satellite   Satellite
    ## 2615            Control  Salad Bar         Main    Satellite   Satellite
    ## 2616            Control  Salad Bar Modification    Satellite   Satellite
    ## 2617            Control       Soup         Main    Satellite   Satellite
    ## 2618            Control       Soup         Main    Satellite   Satellite
    ## 2619            Control  Grab N Go         Main    Satellite   Satellite
    ## 2620            Control  Grab N Go         Main    Satellite   Satellite
    ## 2621            Control Quesadilla         Main    Satellite   Satellite
    ## 2622            Control      Grill         Main    Treatment   Treatment
    ## 2623            Control Quesadilla         Main    Satellite   Satellite
    ## 2624            Control  Grab N Go         Side    Satellite   Satellite
    ## 2625            Control      Grill         Side    Treatment   Treatment
    ## 2626            Control      Grill         Main    Treatment   Treatment
    ## 2627            Control      Grill Modification    Treatment   Treatment
    ## 2628            Control Quesadilla         Main    Satellite   Satellite
    ## 2629            Control      Grill         Main    Treatment   Treatment
    ## 2630            Control      Grill         Main    Treatment   Treatment
    ## 2631            Control  Grab N Go         Side    Satellite   Satellite
    ## 2632            Control      Grill Modification    Treatment   Treatment
    ## 2633            Control      Grill Modification    Treatment   Treatment
    ## 2634            Control      Grill         Main    Treatment   Treatment
    ## 2635            Control      Grill Modification    Treatment   Treatment
    ## 2636            Control      Grill Modification    Treatment   Treatment
    ## 2637            Control        Wok         Main    Satellite   Satellite
    ## 2638            Control      Ramen         Main    Treatment   Treatment
    ## 2639            Control        Wok         Main    Satellite   Satellite
    ## 2640            Control        Wok         Main    Satellite   Satellite
    ## 2641            Control      Ramen         Main    Treatment   Treatment
    ## 2642            Control        Wok         Side    Satellite   Satellite
    ## 2643            Control        Wok         Side    Satellite   Satellite
    ## 2644            Control      Ramen         Main    Treatment   Treatment
    ## 2645            Control        Wok         Side    Satellite   Satellite
    ## 2646            Control        Wok         Side    Satellite   Satellite
    ## 2647            Control      Pasta         Main    Satellite   Satellite
    ## 2648            Control      Pizza         Main    Satellite   Satellite
    ## 2649            Control      Pizza         Main    Satellite   Satellite
    ## 2650            Control      Pasta         Main    Satellite   Satellite
    ## 2651            Control      Pasta Modification    Satellite   Satellite
    ## 2652            Control      Pasta         Side    Satellite   Satellite
    ## 2653            Control  Breakfast         Main    Satellite   Satellite
    ## 2654            Control  Breakfast         Side    Satellite   Satellite
    ## 2655            Control  Breakfast         Main    Satellite   Satellite
    ## 2656            Control  Breakfast Modification    Satellite   Satellite
    ## 2657            Control  Breakfast         Side    Satellite   Satellite
    ## 2658            Control  Breakfast         Side    Satellite   Satellite
    ## 2659            Control  Breakfast         Side    Satellite   Satellite
    ## 2660            Control  Breakfast         Side    Satellite   Satellite
    ## 2661            Control       Wrap         Main    Satellite   Satellite
    ## 2662            Control       Wrap         Main    Satellite   Satellite
    ## 2663            Control       Wrap         Side    Satellite   Satellite
    ## 2664            Control       Wrap Modification    Satellite   Satellite
    ## 2665            Control       Soup         Main    Satellite   Satellite
    ## 2666            Control       Soup         Main    Satellite   Satellite
    ## 2667            Control  Salad Bar         Main    Satellite   Satellite
    ## 2668            Control  Salad Bar Modification    Satellite   Satellite
    ## 2669            Control  Grab N Go         Main    Satellite   Satellite
    ## 2670            Control  Grab N Go         Main    Satellite   Satellite
    ## 2671         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2672         Multimodal      Grill         Main    Treatment   Treatment
    ## 2673         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2674         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2675         Multimodal      Grill         Side    Treatment   Treatment
    ## 2676         Multimodal      Grill         Main    Treatment   Treatment
    ## 2677         Multimodal      Grill         Main    Treatment   Treatment
    ## 2678         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2679         Multimodal      Grill         Main    Treatment   Treatment
    ## 2680         Multimodal      Grill         Side    Treatment   Treatment
    ## 2681         Multimodal      Grill Modification    Treatment   Treatment
    ## 2682         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2683         Multimodal      Grill Modification    Treatment   Treatment
    ## 2684         Multimodal      Grill Modification    Treatment   Treatment
    ## 2685         Multimodal      Grill Modification    Treatment   Treatment
    ## 2686         Multimodal      Grill Modification    Treatment   Treatment
    ## 2687         Multimodal        Wok         Main    Satellite   Satellite
    ## 2688         Multimodal        Wok         Main    Satellite   Satellite
    ## 2689         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2690         Multimodal        Wok         Main    Satellite   Satellite
    ## 2691         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2692         Multimodal        Wok         Main    Satellite   Satellite
    ## 2693         Multimodal        Wok         Side    Satellite   Satellite
    ## 2694         Multimodal        Wok         Side    Satellite   Satellite
    ## 2695         Multimodal        Wok         Side    Satellite   Satellite
    ## 2696         Multimodal        Wok         Side    Satellite   Satellite
    ## 2697         Multimodal        Wok         Side    Satellite   Satellite
    ## 2698         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2699         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2700         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2701         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2702         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2703         Multimodal      Pasta         Side    Satellite   Satellite
    ## 2704         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2705         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2706         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2707         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 2708         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2709         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2710         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2711         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2712         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2713         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2714         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2715         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2716         Multimodal       Wrap         Side    Satellite   Satellite
    ## 2717         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2718         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 2719         Multimodal       Soup         Main    Satellite   Satellite
    ## 2720         Multimodal       Soup         Main    Satellite   Satellite
    ## 2721         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2722         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2723         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2724         Multimodal      Grill         Main    Treatment   Treatment
    ## 2725         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2726         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2727         Multimodal      Grill         Side    Treatment   Treatment
    ## 2728         Multimodal      Grill         Main    Treatment   Treatment
    ## 2729         Multimodal      Grill         Side    Treatment   Treatment
    ## 2730         Multimodal      Grill Modification    Treatment   Treatment
    ## 2731         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2732         Multimodal      Grill         Main    Treatment   Treatment
    ## 2733         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2734         Multimodal      Grill Modification    Treatment   Treatment
    ## 2735         Multimodal      Grill         Main    Treatment   Treatment
    ## 2736         Multimodal      Grill         Main    Treatment   Treatment
    ## 2737         Multimodal      Grill Modification    Treatment   Treatment
    ## 2738         Multimodal      Grill Modification    Treatment   Treatment
    ## 2739         Multimodal      Grill Modification    Treatment   Treatment
    ## 2740         Multimodal      Grill Modification    Treatment   Treatment
    ## 2741         Multimodal        Wok         Main    Satellite   Satellite
    ## 2742         Multimodal        Wok         Main    Satellite   Satellite
    ## 2743         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2744         Multimodal        Wok         Main    Satellite   Satellite
    ## 2745         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2746         Multimodal        Wok         Main    Satellite   Satellite
    ## 2747         Multimodal        Wok         Side    Satellite   Satellite
    ## 2748         Multimodal        Wok         Side    Satellite   Satellite
    ## 2749         Multimodal        Wok         Side    Satellite   Satellite
    ## 2750         Multimodal        Wok         Side    Satellite   Satellite
    ## 2751         Multimodal        Wok         Side    Satellite   Satellite
    ## 2752         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2753         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2754         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2755         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2756         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2757         Multimodal      Pasta         Side    Satellite   Satellite
    ## 2758         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2759         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2760         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2761         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 2762         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2763         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2764         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2765         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2766         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2767         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2768         Multimodal       Wrap         Side    Satellite   Satellite
    ## 2769         Multimodal       Soup         Main    Satellite   Satellite
    ## 2770         Multimodal       Soup         Main    Satellite   Satellite
    ## 2771         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2772         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 2773         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2774         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2775         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2776         Multimodal      Grill         Main    Treatment   Treatment
    ## 2777         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2778         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2779         Multimodal      Grill         Side    Treatment   Treatment
    ## 2780         Multimodal      Grill         Main    Treatment   Treatment
    ## 2781         Multimodal      Grill Modification    Treatment   Treatment
    ## 2782         Multimodal      Grill         Side    Treatment   Treatment
    ## 2783         Multimodal      Grill         Main    Treatment   Treatment
    ## 2784         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2785         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2786         Multimodal      Grill         Main    Treatment   Treatment
    ## 2787         Multimodal      Grill         Main    Treatment   Treatment
    ## 2788         Multimodal      Grill Modification    Treatment   Treatment
    ## 2789         Multimodal      Grill Modification    Treatment   Treatment
    ## 2790         Multimodal      Grill Modification    Treatment   Treatment
    ## 2791         Multimodal      Grill Modification    Treatment   Treatment
    ## 2792         Multimodal        Wok         Main    Satellite   Satellite
    ## 2793         Multimodal        Wok         Main    Satellite   Satellite
    ## 2794         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2795         Multimodal        Wok         Main    Satellite   Satellite
    ## 2796         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2797         Multimodal        Wok         Main    Satellite   Satellite
    ## 2798         Multimodal        Wok         Side    Satellite   Satellite
    ## 2799         Multimodal        Wok         Side    Satellite   Satellite
    ## 2800         Multimodal        Wok         Side    Satellite   Satellite
    ## 2801         Multimodal        Wok         Side    Satellite   Satellite
    ## 2802         Multimodal        Wok         Side    Satellite   Satellite
    ## 2803         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2804         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2805         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2806         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2807         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2808         Multimodal      Pasta         Side    Satellite   Satellite
    ## 2809         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2810         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2811         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2812         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 2813         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2814         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2815         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2816         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2817         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2818         Multimodal       Wrap         Side    Satellite   Satellite
    ## 2819         Multimodal       Wrap         Side    Satellite   Satellite
    ## 2820         Multimodal       Wrap Modification    Satellite   Satellite
    ## 2821         Multimodal       Soup         Main    Satellite   Satellite
    ## 2822         Multimodal       Soup         Main    Satellite   Satellite
    ## 2823         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2824         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 2825         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2826         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2827         Multimodal        Wok         Main    Satellite   Satellite
    ## 2828         Multimodal        Wok         Main    Satellite   Satellite
    ## 2829         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2830         Multimodal        Wok         Main    Satellite   Satellite
    ## 2831         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2832         Multimodal        Wok         Main    Satellite   Satellite
    ## 2833         Multimodal        Wok         Side    Satellite   Satellite
    ## 2834         Multimodal        Wok         Side    Satellite   Satellite
    ## 2835         Multimodal        Wok         Side    Satellite   Satellite
    ## 2836         Multimodal        Wok         Side    Satellite   Satellite
    ## 2837         Multimodal        Wok         Side    Satellite   Satellite
    ## 2838         Multimodal        Wok         Side    Satellite   Satellite
    ## 2839         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2840         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2841         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2842         Multimodal      Grill         Side    Treatment   Treatment
    ## 2843         Multimodal      Grill         Side    Treatment   Treatment
    ## 2844         Multimodal      Grill Modification    Treatment   Treatment
    ## 2845         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2846         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2847         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2848         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2849         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2850         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2851         Multimodal      Pasta         Side    Satellite   Satellite
    ## 2852         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2853         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2854         Multimodal       Wrap Modification    Satellite   Satellite
    ## 2855         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2856         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2857         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2858         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2859         Multimodal       Soup         Main    Satellite   Satellite
    ## 2860         Multimodal       Soup         Main    Satellite   Satellite
    ## 2861         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2862         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 2863         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2864         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2865         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2866         Multimodal      Grill         Main    Treatment   Treatment
    ## 2867         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2868         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2869         Multimodal      Grill         Side    Treatment   Treatment
    ## 2870         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2871         Multimodal      Grill         Side    Treatment   Treatment
    ## 2872         Multimodal      Grill Modification    Treatment   Treatment
    ## 2873         Multimodal      Grill         Main    Treatment   Treatment
    ## 2874         Multimodal      Grill         Main    Treatment   Treatment
    ## 2875         Multimodal      Grill         Main    Treatment   Treatment
    ## 2876         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2877         Multimodal      Grill         Main    Treatment   Treatment
    ## 2878         Multimodal      Grill Modification    Treatment   Treatment
    ## 2879         Multimodal      Grill Modification    Treatment   Treatment
    ## 2880         Multimodal      Grill Modification    Treatment   Treatment
    ## 2881         Multimodal        Wok         Main    Satellite   Satellite
    ## 2882         Multimodal        Wok         Main    Satellite   Satellite
    ## 2883         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2884         Multimodal        Wok         Main    Satellite   Satellite
    ## 2885         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2886         Multimodal        Wok         Side    Satellite   Satellite
    ## 2887         Multimodal        Wok         Side    Satellite   Satellite
    ## 2888         Multimodal        Wok         Main    Satellite   Satellite
    ## 2889         Multimodal        Wok         Side    Satellite   Satellite
    ## 2890         Multimodal        Wok         Side    Satellite   Satellite
    ## 2891         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2892         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2893         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2894         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 2895         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2896         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2897         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2898         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2899         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2900         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2901         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2902         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2903         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2904         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2905         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2906         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2907         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2908         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2909         Multimodal       Soup         Main    Satellite   Satellite
    ## 2910         Multimodal       Soup         Main    Satellite   Satellite
    ## 2911         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2912         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2913         Multimodal      Grill         Main    Treatment   Treatment
    ## 2914         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2915         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2916         Multimodal      Grill         Side    Treatment   Treatment
    ## 2917         Multimodal      Grill         Side    Treatment   Treatment
    ## 2918         Multimodal      Grill         Main    Treatment   Treatment
    ## 2919         Multimodal      Grill         Main    Treatment   Treatment
    ## 2920         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2921         Multimodal      Grill Modification    Treatment   Treatment
    ## 2922         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2923         Multimodal      Grill         Main    Treatment   Treatment
    ## 2924         Multimodal      Grill         Main    Treatment   Treatment
    ## 2925         Multimodal      Grill Modification    Treatment   Treatment
    ## 2926         Multimodal      Grill Modification    Treatment   Treatment
    ## 2927         Multimodal      Grill Modification    Treatment   Treatment
    ## 2928         Multimodal      Grill Modification    Treatment   Treatment
    ## 2929         Multimodal        Wok         Main    Satellite   Satellite
    ## 2930         Multimodal        Wok         Main    Satellite   Satellite
    ## 2931         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2932         Multimodal        Wok         Main    Satellite   Satellite
    ## 2933         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2934         Multimodal        Wok         Side    Satellite   Satellite
    ## 2935         Multimodal        Wok         Main    Satellite   Satellite
    ## 2936         Multimodal        Wok         Side    Satellite   Satellite
    ## 2937         Multimodal        Wok         Side    Satellite   Satellite
    ## 2938         Multimodal        Wok         Side    Satellite   Satellite
    ## 2939         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2940         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2941         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2942         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2943         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2944         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2945         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2946         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2947         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 2948         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2949         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2950         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2951         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2952         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2953         Multimodal       Wrap         Main    Satellite   Satellite
    ## 2954         Multimodal       Wrap         Side    Satellite   Satellite
    ## 2955         Multimodal       Wrap Modification    Satellite   Satellite
    ## 2956         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2957         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2958         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 2959         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 2960         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 2961         Multimodal       Soup         Main    Satellite   Satellite
    ## 2962         Multimodal       Soup         Main    Satellite   Satellite
    ## 2963         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2964         Multimodal      Grill         Main    Treatment   Treatment
    ## 2965         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2966         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2967         Multimodal      Grill         Side    Treatment   Treatment
    ## 2968         Multimodal      Grill         Main    Treatment   Treatment
    ## 2969         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 2970         Multimodal      Grill         Side    Treatment   Treatment
    ## 2971         Multimodal      Grill Modification    Treatment   Treatment
    ## 2972         Multimodal      Grill         Main    Treatment   Treatment
    ## 2973         Multimodal      Grill         Main    Treatment   Treatment
    ## 2974         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 2975         Multimodal      Grill Modification    Treatment   Treatment
    ## 2976         Multimodal      Grill         Main    Treatment   Treatment
    ## 2977         Multimodal      Grill Modification    Treatment   Treatment
    ## 2978         Multimodal      Grill Modification    Treatment   Treatment
    ## 2979         Multimodal      Grill Modification    Treatment   Treatment
    ## 2980         Multimodal        Wok         Main    Satellite   Satellite
    ## 2981         Multimodal        Wok         Main    Satellite   Satellite
    ## 2982         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2983         Multimodal        Wok         Main    Satellite   Satellite
    ## 2984         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2985         Multimodal        Wok         Side    Satellite   Satellite
    ## 2986         Multimodal        Wok         Side    Satellite   Satellite
    ## 2987         Multimodal        Wok         Side    Satellite   Satellite
    ## 2988         Multimodal        Wok         Main    Satellite   Satellite
    ## 2989         Multimodal        Wok         Side    Satellite   Satellite
    ## 2990         Multimodal      Ramen         Main    Treatment   Treatment
    ## 2991         Multimodal        Wok         Side    Satellite   Satellite
    ## 2992         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2993         Multimodal      Pasta         Main    Satellite   Satellite
    ## 2994         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2995         Multimodal      Pizza         Main    Satellite   Satellite
    ## 2996         Multimodal      Pasta Modification    Satellite   Satellite
    ## 2997         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 2998         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 2999         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3000         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3001         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3002         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3003         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3004         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3005         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3006         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3007         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3008         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3009         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3010         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3011         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3012         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3013         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3014         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3015         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3016         Multimodal       Soup         Main    Satellite   Satellite
    ## 3017         Multimodal       Soup         Main    Satellite   Satellite
    ## 3018         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3019         Multimodal      Grill         Main    Treatment   Treatment
    ## 3020         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3021         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3022         Multimodal      Grill         Side    Treatment   Treatment
    ## 3023         Multimodal      Grill         Main    Treatment   Treatment
    ## 3024         Multimodal      Grill         Side    Treatment   Treatment
    ## 3025         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3026         Multimodal      Grill         Main    Treatment   Treatment
    ## 3027         Multimodal      Grill         Main    Treatment   Treatment
    ## 3028         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3029         Multimodal      Grill Modification    Treatment   Treatment
    ## 3030         Multimodal      Grill Modification    Treatment   Treatment
    ## 3031         Multimodal      Grill         Main    Treatment   Treatment
    ## 3032         Multimodal      Grill Modification    Treatment   Treatment
    ## 3033         Multimodal      Grill Modification    Treatment   Treatment
    ## 3034         Multimodal      Grill Modification    Treatment   Treatment
    ## 3035         Multimodal        Wok         Main    Satellite   Satellite
    ## 3036         Multimodal        Wok         Main    Satellite   Satellite
    ## 3037         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3038         Multimodal        Wok         Main    Satellite   Satellite
    ## 3039         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3040         Multimodal        Wok         Main    Satellite   Satellite
    ## 3041         Multimodal        Wok         Side    Satellite   Satellite
    ## 3042         Multimodal        Wok         Side    Satellite   Satellite
    ## 3043         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3044         Multimodal        Wok         Side    Satellite   Satellite
    ## 3045         Multimodal        Wok         Side    Satellite   Satellite
    ## 3046         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3047         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3048         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3049         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3050         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3051         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3052         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3053         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3054         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3055         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3056         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3057         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3058         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3059         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3060         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3061         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3062         Multimodal       Wrap Modification    Satellite   Satellite
    ## 3063         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3064         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3065         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3066         Multimodal       Soup         Main    Satellite   Satellite
    ## 3067         Multimodal       Soup         Main    Satellite   Satellite
    ## 3068         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3069         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3070         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3071         Multimodal      Grill         Main    Treatment   Treatment
    ## 3072         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3073         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3074         Multimodal      Grill         Side    Treatment   Treatment
    ## 3075         Multimodal      Grill         Main    Treatment   Treatment
    ## 3076         Multimodal      Grill         Main    Treatment   Treatment
    ## 3077         Multimodal      Grill         Main    Treatment   Treatment
    ## 3078         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3079         Multimodal      Grill         Side    Treatment   Treatment
    ## 3080         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3081         Multimodal      Grill Modification    Treatment   Treatment
    ## 3082         Multimodal      Grill         Main    Treatment   Treatment
    ## 3083         Multimodal      Grill Modification    Treatment   Treatment
    ## 3084         Multimodal      Grill Modification    Treatment   Treatment
    ## 3085         Multimodal      Grill Modification    Treatment   Treatment
    ## 3086         Multimodal      Grill Modification    Treatment   Treatment
    ## 3087         Multimodal        Wok         Main    Satellite   Satellite
    ## 3088         Multimodal        Wok         Main    Satellite   Satellite
    ## 3089         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3090         Multimodal        Wok         Main    Satellite   Satellite
    ## 3091         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3092         Multimodal        Wok         Main    Satellite   Satellite
    ## 3093         Multimodal        Wok         Side    Satellite   Satellite
    ## 3094         Multimodal        Wok         Side    Satellite   Satellite
    ## 3095         Multimodal        Wok         Side    Satellite   Satellite
    ## 3096         Multimodal        Wok         Side    Satellite   Satellite
    ## 3097         Multimodal        Wok         Side    Satellite   Satellite
    ## 3098         Multimodal        Wok         Side    Satellite   Satellite
    ## 3099         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3100         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3101         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3102         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3103         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3104         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3105         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3106         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3107         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3108         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3109         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3110         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3111         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3112         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3113         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3114         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3115         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3116         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3117         Multimodal       Wrap Modification    Satellite   Satellite
    ## 3118         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3119         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3120         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3121         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3122         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3123         Multimodal       Soup         Main    Satellite   Satellite
    ## 3124         Multimodal       Soup         Main    Satellite   Satellite
    ## 3125         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3126         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3127         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3128         Multimodal      Grill         Main    Treatment   Treatment
    ## 3129         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3130         Multimodal      Grill         Side    Treatment   Treatment
    ## 3131         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3132         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3133         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3134         Multimodal      Grill         Main    Treatment   Treatment
    ## 3135         Multimodal      Grill         Main    Treatment   Treatment
    ## 3136         Multimodal      Grill         Main    Treatment   Treatment
    ## 3137         Multimodal      Grill         Main    Treatment   Treatment
    ## 3138         Multimodal      Grill Modification    Treatment   Treatment
    ## 3139         Multimodal      Grill Modification    Treatment   Treatment
    ## 3140         Multimodal        Wok         Main    Satellite   Satellite
    ## 3141         Multimodal        Wok         Main    Satellite   Satellite
    ## 3142         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3143         Multimodal        Wok         Main    Satellite   Satellite
    ## 3144         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3145         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3146         Multimodal        Wok         Side    Satellite   Satellite
    ## 3147         Multimodal        Wok         Side    Satellite   Satellite
    ## 3148         Multimodal        Wok         Main    Satellite   Satellite
    ## 3149         Multimodal        Wok         Side    Satellite   Satellite
    ## 3150         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3151         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3152         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3153         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3154         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3155         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3156         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3157         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3158         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3159         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3160         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3161         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3162         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3163         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3164         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3165         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3166         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3167         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3168         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3169         Multimodal       Soup         Main    Satellite   Satellite
    ## 3170         Multimodal       Soup         Main    Satellite   Satellite
    ## 3171         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3172         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3173         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3174         Multimodal      Grill         Main    Treatment   Treatment
    ## 3175         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3176         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3177         Multimodal      Grill         Side    Treatment   Treatment
    ## 3178         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3179         Multimodal      Grill         Main    Treatment   Treatment
    ## 3180         Multimodal      Grill Modification    Treatment   Treatment
    ## 3181         Multimodal      Grill         Side    Treatment   Treatment
    ## 3182         Multimodal      Grill         Main    Treatment   Treatment
    ## 3183         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3184         Multimodal      Grill         Main    Treatment   Treatment
    ## 3185         Multimodal      Grill         Main    Treatment   Treatment
    ## 3186         Multimodal      Grill Modification    Treatment   Treatment
    ## 3187         Multimodal      Grill Modification    Treatment   Treatment
    ## 3188         Multimodal      Grill Modification    Treatment   Treatment
    ## 3189         Multimodal      Grill Modification    Treatment   Treatment
    ## 3190         Multimodal        Wok         Main    Satellite   Satellite
    ## 3191         Multimodal        Wok         Main    Satellite   Satellite
    ## 3192         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3193         Multimodal        Wok         Main    Satellite   Satellite
    ## 3194         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3195         Multimodal        Wok         Main    Satellite   Satellite
    ## 3196         Multimodal        Wok         Side    Satellite   Satellite
    ## 3197         Multimodal        Wok         Side    Satellite   Satellite
    ## 3198         Multimodal        Wok         Side    Satellite   Satellite
    ## 3199         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3200         Multimodal        Wok         Side    Satellite   Satellite
    ## 3201         Multimodal        Wok         Side    Satellite   Satellite
    ## 3202         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3203         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3204         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3205         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3206         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3207         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3208         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3209         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3210         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3211         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3212         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3213         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3214         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3215         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3216         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3217         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3218         Multimodal       Soup         Main    Satellite   Satellite
    ## 3219         Multimodal       Soup         Main    Satellite   Satellite
    ## 3220         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3221         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3222         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3223         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3224         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3225         Multimodal       Deli         Main    Satellite   Satellite
    ## 3226         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3227         Multimodal      Grill         Main    Treatment   Treatment
    ## 3228         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3229         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3230         Multimodal      Grill         Side    Treatment   Treatment
    ## 3231         Multimodal      Grill         Main    Treatment   Treatment
    ## 3232         Multimodal      Grill         Side    Treatment   Treatment
    ## 3233         Multimodal      Grill         Main    Treatment   Treatment
    ## 3234         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3235         Multimodal      Grill Modification    Treatment   Treatment
    ## 3236         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3237         Multimodal      Grill         Main    Treatment   Treatment
    ## 3238         Multimodal      Grill         Main    Treatment   Treatment
    ## 3239         Multimodal      Grill Modification    Treatment   Treatment
    ## 3240         Multimodal      Grill Modification    Treatment   Treatment
    ## 3241         Multimodal      Grill Modification    Treatment   Treatment
    ## 3242         Multimodal      Grill Modification    Treatment   Treatment
    ## 3243         Multimodal        Wok         Main    Satellite   Satellite
    ## 3244         Multimodal        Wok         Main    Satellite   Satellite
    ## 3245         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3246         Multimodal        Wok         Main    Satellite   Satellite
    ## 3247         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3248         Multimodal        Wok         Side    Satellite   Satellite
    ## 3249         Multimodal        Wok         Main    Satellite   Satellite
    ## 3250         Multimodal        Wok         Side    Satellite   Satellite
    ## 3251         Multimodal        Wok         Side    Satellite   Satellite
    ## 3252         Multimodal        Wok         Side    Satellite   Satellite
    ## 3253         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3254         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3255         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3256         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3257         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3258         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3259         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3260         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3261         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3262         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3263         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3264         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3265         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3266         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3267         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3268         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3269         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3270         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3271         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3272         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3273         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3274         Multimodal       Soup         Main    Satellite   Satellite
    ## 3275         Multimodal       Soup         Main    Satellite   Satellite
    ## 3276         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3277         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3278         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3279         Multimodal      Grill         Main    Treatment   Treatment
    ## 3280         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3281         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3282         Multimodal      Grill         Side    Treatment   Treatment
    ## 3283         Multimodal      Grill         Main    Treatment   Treatment
    ## 3284         Multimodal Quesadilla         Main    Satellite   Satellite
    ## 3285         Multimodal      Grill         Side    Treatment   Treatment
    ## 3286         Multimodal      Grill         Main    Treatment   Treatment
    ## 3287         Multimodal      Grill Modification    Treatment   Treatment
    ## 3288         Multimodal  Grab N Go         Side    Satellite   Satellite
    ## 3289         Multimodal      Grill         Main    Treatment   Treatment
    ## 3290         Multimodal      Grill         Main    Treatment   Treatment
    ## 3291         Multimodal      Grill Modification    Treatment   Treatment
    ## 3292         Multimodal      Grill Modification    Treatment   Treatment
    ## 3293         Multimodal        Wok         Main    Satellite   Satellite
    ## 3294         Multimodal        Wok         Main    Satellite   Satellite
    ## 3295         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3296         Multimodal        Wok         Main    Satellite   Satellite
    ## 3297         Multimodal      Ramen         Main    Treatment   Treatment
    ## 3298         Multimodal        Wok         Main    Satellite   Satellite
    ## 3299         Multimodal        Wok         Side    Satellite   Satellite
    ## 3300         Multimodal        Wok         Side    Satellite   Satellite
    ## 3301         Multimodal        Wok         Side    Satellite   Satellite
    ## 3302         Multimodal        Wok         Side    Satellite   Satellite
    ## 3303         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3304         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3305         Multimodal      Pasta         Main    Satellite   Satellite
    ## 3306         Multimodal      Pizza         Main    Satellite   Satellite
    ## 3307         Multimodal      Pasta Modification    Satellite   Satellite
    ## 3308         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3309         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3310         Multimodal  Breakfast         Main    Satellite   Satellite
    ## 3311         Multimodal  Breakfast Modification    Satellite   Satellite
    ## 3312         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3313         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3314         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3315         Multimodal  Breakfast         Side    Satellite   Satellite
    ## 3316         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3317         Multimodal       Wrap         Main    Satellite   Satellite
    ## 3318         Multimodal       Wrap         Side    Satellite   Satellite
    ## 3319         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3320         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3321         Multimodal  Grab N Go         Main    Satellite   Satellite
    ## 3322         Multimodal       Soup         Main    Satellite   Satellite
    ## 3323         Multimodal       Soup         Main    Satellite   Satellite
    ## 3324         Multimodal  Salad Bar         Main    Satellite   Satellite
    ## 3325         Multimodal  Salad Bar Modification    Satellite   Satellite
    ## 3326            Control Quesadilla         Main    Satellite   Satellite
    ## 3327            Control      Grill         Main    Treatment   Treatment
    ## 3328            Control  Grab N Go         Side    Satellite   Satellite
    ## 3329            Control Quesadilla         Main    Satellite   Satellite
    ## 3330            Control      Grill         Side    Treatment   Treatment
    ## 3331            Control Quesadilla         Main    Satellite   Satellite
    ## 3332            Control      Grill         Main    Treatment   Treatment
    ## 3333            Control      Grill         Main    Treatment   Treatment
    ## 3334            Control      Grill Modification    Treatment   Treatment
    ## 3335            Control      Grill         Side    Treatment   Treatment
    ## 3336            Control      Grill         Main    Treatment   Treatment
    ## 3337            Control  Grab N Go         Side    Satellite   Satellite
    ## 3338            Control      Grill Modification    Treatment   Treatment
    ## 3339            Control      Grill         Main    Treatment   Treatment
    ## 3340            Control      Grill Modification    Treatment   Treatment
    ## 3341            Control      Grill Modification    Treatment   Treatment
    ## 3342            Control      Grill Modification    Treatment   Treatment
    ## 3343            Control        Wok         Main    Satellite   Satellite
    ## 3344            Control        Wok         Main    Satellite   Satellite
    ## 3345            Control      Ramen         Main    Treatment   Treatment
    ## 3346            Control        Wok         Main    Satellite   Satellite
    ## 3347            Control      Ramen         Main    Treatment   Treatment
    ## 3348            Control        Wok         Main    Satellite   Satellite
    ## 3349            Control        Wok         Side    Satellite   Satellite
    ## 3350            Control        Wok         Side    Satellite   Satellite
    ## 3351            Control        Wok         Side    Satellite   Satellite
    ## 3352            Control        Wok         Side    Satellite   Satellite
    ## 3353            Control      Pasta         Main    Satellite   Satellite
    ## 3354            Control      Pasta         Main    Satellite   Satellite
    ## 3355            Control      Pizza         Main    Satellite   Satellite
    ## 3356            Control      Pasta Modification    Satellite   Satellite
    ## 3357            Control      Pizza         Main    Satellite   Satellite
    ## 3358            Control  Breakfast         Main    Satellite   Satellite
    ## 3359            Control  Breakfast         Side    Satellite   Satellite
    ## 3360            Control  Breakfast         Main    Satellite   Satellite
    ## 3361            Control  Breakfast Modification    Satellite   Satellite
    ## 3362            Control  Breakfast         Side    Satellite   Satellite
    ## 3363            Control  Breakfast         Side    Satellite   Satellite
    ## 3364            Control  Breakfast         Side    Satellite   Satellite
    ## 3365            Control  Breakfast         Side    Satellite   Satellite
    ## 3366            Control  Breakfast         Side    Satellite   Satellite
    ## 3367            Control       Wrap         Main    Satellite   Satellite
    ## 3368            Control       Wrap         Main    Satellite   Satellite
    ## 3369            Control       Wrap         Side    Satellite   Satellite
    ## 3370            Control       Wrap Modification    Satellite   Satellite
    ## 3371            Control  Salad Bar         Main    Satellite   Satellite
    ## 3372            Control  Salad Bar Modification    Satellite   Satellite
    ## 3373            Control  Grab N Go         Main    Satellite   Satellite
    ## 3374            Control  Grab N Go         Main    Satellite   Satellite
    ## 3375            Control  Grab N Go         Main    Satellite   Satellite
    ## 3376            Control       Soup         Main    Satellite   Satellite
    ## 3377            Control       Soup         Main    Satellite   Satellite
    ## 3378            Control       Deli         Main    Satellite   Satellite
    ## 3379            Control Quesadilla         Main    Satellite   Satellite
    ## 3380            Control      Grill         Main    Treatment   Treatment
    ## 3381            Control  Grab N Go         Side    Satellite   Satellite
    ## 3382            Control Quesadilla         Main    Satellite   Satellite
    ## 3383            Control      Grill         Side    Treatment   Treatment
    ## 3384            Control Quesadilla         Main    Satellite   Satellite
    ## 3385            Control      Grill         Main    Treatment   Treatment
    ## 3386            Control      Grill         Side    Treatment   Treatment
    ## 3387            Control      Grill         Main    Treatment   Treatment
    ## 3388            Control      Grill         Main    Treatment   Treatment
    ## 3389            Control      Grill         Main    Treatment   Treatment
    ## 3390            Control  Grab N Go         Side    Satellite   Satellite
    ## 3391            Control      Grill Modification    Treatment   Treatment
    ## 3392            Control      Grill Modification    Treatment   Treatment
    ## 3393            Control      Grill Modification    Treatment   Treatment
    ## 3394            Control      Grill Modification    Treatment   Treatment
    ## 3395            Control        Wok         Main    Satellite   Satellite
    ## 3396            Control      Ramen         Main    Treatment   Treatment
    ## 3397            Control        Wok         Main    Satellite   Satellite
    ## 3398            Control        Wok         Main    Satellite   Satellite
    ## 3399            Control      Ramen         Main    Treatment   Treatment
    ## 3400            Control        Wok         Main    Satellite   Satellite
    ## 3401            Control        Wok         Side    Satellite   Satellite
    ## 3402            Control        Wok         Side    Satellite   Satellite
    ## 3403            Control        Wok         Side    Satellite   Satellite
    ## 3404            Control        Wok         Side    Satellite   Satellite
    ## 3405            Control      Pasta         Main    Satellite   Satellite
    ## 3406            Control      Pizza         Main    Satellite   Satellite
    ## 3407            Control      Pasta         Main    Satellite   Satellite
    ## 3408            Control      Pizza         Main    Satellite   Satellite
    ## 3409            Control      Pasta Modification    Satellite   Satellite
    ## 3410            Control      Pasta         Side    Satellite   Satellite
    ## 3411            Control  Breakfast         Main    Satellite   Satellite
    ## 3412            Control  Breakfast         Side    Satellite   Satellite
    ## 3413            Control  Breakfast         Main    Satellite   Satellite
    ## 3414            Control  Breakfast Modification    Satellite   Satellite
    ## 3415            Control  Breakfast         Side    Satellite   Satellite
    ## 3416            Control  Breakfast         Side    Satellite   Satellite
    ## 3417            Control  Breakfast         Side    Satellite   Satellite
    ## 3418            Control  Breakfast         Side    Satellite   Satellite
    ## 3419            Control       Wrap         Main    Satellite   Satellite
    ## 3420            Control       Wrap         Main    Satellite   Satellite
    ## 3421            Control       Wrap         Side    Satellite   Satellite
    ## 3422            Control       Wrap Modification    Satellite   Satellite
    ## 3423            Control       Wrap         Side    Satellite   Satellite
    ## 3424            Control  Grab N Go         Main    Satellite   Satellite
    ## 3425            Control  Grab N Go         Main    Satellite   Satellite
    ## 3426            Control  Grab N Go         Main    Satellite   Satellite
    ## 3427            Control  Grab N Go         Main    Satellite   Satellite
    ## 3428            Control  Salad Bar         Main    Satellite   Satellite
    ## 3429            Control       Soup         Main    Satellite   Satellite
    ## 3430            Control       Soup         Main    Satellite   Satellite
    ## 3431            Control       Deli         Main    Satellite   Satellite
    ## 3432            Control Quesadilla         Main    Satellite   Satellite
    ## 3433            Control      Grill         Main    Treatment   Treatment
    ## 3434            Control  Grab N Go         Side    Satellite   Satellite
    ## 3435            Control Quesadilla         Main    Satellite   Satellite
    ## 3436            Control      Grill         Side    Treatment   Treatment
    ## 3437            Control      Grill         Main    Treatment   Treatment
    ## 3438            Control Quesadilla         Main    Satellite   Satellite
    ## 3439            Control      Grill         Main    Treatment   Treatment
    ## 3440            Control      Grill         Side    Treatment   Treatment
    ## 3441            Control      Grill Modification    Treatment   Treatment
    ## 3442            Control  Grab N Go         Side    Satellite   Satellite
    ## 3443            Control      Grill         Main    Treatment   Treatment
    ## 3444            Control      Grill         Main    Treatment   Treatment
    ## 3445            Control      Grill Modification    Treatment   Treatment
    ## 3446            Control      Grill Modification    Treatment   Treatment
    ## 3447            Control      Grill Modification    Treatment   Treatment
    ## 3448            Control      Grill Modification    Treatment   Treatment
    ## 3449            Control        Wok         Main    Satellite   Satellite
    ## 3450            Control        Wok         Main    Satellite   Satellite
    ## 3451            Control      Ramen         Main    Treatment   Treatment
    ## 3452            Control        Wok         Main    Satellite   Satellite
    ## 3453            Control      Ramen         Main    Treatment   Treatment
    ## 3454            Control        Wok         Main    Satellite   Satellite
    ## 3455            Control        Wok         Side    Satellite   Satellite
    ## 3456            Control        Wok         Side    Satellite   Satellite
    ## 3457            Control        Wok         Side    Satellite   Satellite
    ## 3458            Control        Wok         Side    Satellite   Satellite
    ## 3459            Control      Pasta         Main    Satellite   Satellite
    ## 3460            Control      Pizza         Main    Satellite   Satellite
    ## 3461            Control      Pasta         Main    Satellite   Satellite
    ## 3462            Control      Pizza         Main    Satellite   Satellite
    ## 3463            Control      Pasta Modification    Satellite   Satellite
    ## 3464            Control      Pasta         Side    Satellite   Satellite
    ## 3465            Control  Breakfast         Main    Satellite   Satellite
    ## 3466            Control  Breakfast         Side    Satellite   Satellite
    ## 3467            Control  Breakfast         Main    Satellite   Satellite
    ## 3468            Control  Breakfast Modification    Satellite   Satellite
    ## 3469            Control  Breakfast         Side    Satellite   Satellite
    ## 3470            Control  Breakfast         Side    Satellite   Satellite
    ## 3471            Control  Breakfast         Side    Satellite   Satellite
    ## 3472            Control  Breakfast         Side    Satellite   Satellite
    ## 3473            Control  Breakfast         Side    Satellite   Satellite
    ## 3474            Control  Breakfast         Side    Satellite   Satellite
    ## 3475            Control       Wrap         Main    Satellite   Satellite
    ## 3476            Control       Wrap         Main    Satellite   Satellite
    ## 3477            Control       Wrap         Side    Satellite   Satellite
    ## 3478            Control       Soup         Main    Satellite   Satellite
    ## 3479            Control       Soup         Main    Satellite   Satellite
    ## 3480            Control  Grab N Go         Main    Satellite   Satellite
    ## 3481            Control  Grab N Go         Main    Satellite   Satellite
    ## 3482            Control  Grab N Go         Main    Satellite   Satellite
    ## 3483            Control  Salad Bar         Main    Satellite   Satellite
    ## 3484            Control  Salad Bar Modification    Satellite   Satellite
    ## 3485            Control Quesadilla         Main    Satellite   Satellite
    ## 3486            Control      Grill         Main    Treatment   Treatment
    ## 3487            Control  Grab N Go         Side    Satellite   Satellite
    ## 3488            Control Quesadilla         Main    Satellite   Satellite
    ## 3489            Control      Grill         Side    Treatment   Treatment
    ## 3490            Control Quesadilla         Main    Satellite   Satellite
    ## 3491            Control      Grill         Main    Treatment   Treatment
    ## 3492            Control      Grill         Main    Treatment   Treatment
    ## 3493            Control      Grill         Side    Treatment   Treatment
    ## 3494            Control      Grill         Main    Treatment   Treatment
    ## 3495            Control  Grab N Go         Side    Satellite   Satellite
    ## 3496            Control      Grill Modification    Treatment   Treatment
    ## 3497            Control      Grill         Main    Treatment   Treatment
    ## 3498            Control      Grill Modification    Treatment   Treatment
    ## 3499            Control      Grill Modification    Treatment   Treatment
    ## 3500            Control      Grill Modification    Treatment   Treatment
    ## 3501            Control      Grill Modification    Treatment   Treatment
    ## 3502            Control      Grill Modification    Treatment   Treatment
    ## 3503            Control        Wok         Main    Satellite   Satellite
    ## 3504            Control        Wok         Main    Satellite   Satellite
    ## 3505            Control      Ramen         Main    Treatment   Treatment
    ## 3506            Control        Wok         Main    Satellite   Satellite
    ## 3507            Control      Ramen         Main    Treatment   Treatment
    ## 3508            Control        Wok         Side    Satellite   Satellite
    ## 3509            Control        Wok         Main    Satellite   Satellite
    ## 3510            Control        Wok         Side    Satellite   Satellite
    ## 3511            Control        Wok         Side    Satellite   Satellite
    ## 3512            Control        Wok         Side    Satellite   Satellite
    ## 3513            Control      Pasta         Main    Satellite   Satellite
    ## 3514            Control      Pasta         Main    Satellite   Satellite
    ## 3515            Control      Pizza         Main    Satellite   Satellite
    ## 3516            Control      Pizza         Main    Satellite   Satellite
    ## 3517            Control      Pasta Modification    Satellite   Satellite
    ## 3518            Control  Breakfast         Main    Satellite   Satellite
    ## 3519            Control  Breakfast         Side    Satellite   Satellite
    ## 3520            Control  Breakfast         Main    Satellite   Satellite
    ## 3521            Control  Breakfast Modification    Satellite   Satellite
    ## 3522            Control  Breakfast         Side    Satellite   Satellite
    ## 3523            Control  Breakfast         Side    Satellite   Satellite
    ## 3524            Control  Breakfast         Side    Satellite   Satellite
    ## 3525            Control  Breakfast         Side    Satellite   Satellite
    ## 3526            Control  Breakfast         Side    Satellite   Satellite
    ## 3527            Control       Wrap         Main    Satellite   Satellite
    ## 3528            Control       Wrap         Main    Satellite   Satellite
    ## 3529            Control       Wrap         Side    Satellite   Satellite
    ## 3530            Control       Wrap Modification    Satellite   Satellite
    ## 3531            Control  Grab N Go         Main    Satellite   Satellite
    ## 3532            Control  Grab N Go         Main    Satellite   Satellite
    ## 3533            Control  Grab N Go         Main    Satellite   Satellite
    ## 3534            Control  Grab N Go         Main    Satellite   Satellite
    ## 3535            Control       Soup         Main    Satellite   Satellite
    ## 3536            Control       Soup         Main    Satellite   Satellite
    ## 3537            Control  Salad Bar         Main    Satellite   Satellite
    ## 3538            Control Quesadilla         Main    Satellite   Satellite
    ## 3539            Control      Grill         Main    Treatment   Treatment
    ## 3540            Control  Grab N Go         Side    Satellite   Satellite
    ## 3541            Control Quesadilla         Main    Satellite   Satellite
    ## 3542            Control      Grill         Side    Treatment   Treatment
    ## 3543            Control      Grill         Main    Treatment   Treatment
    ## 3544            Control      Grill         Side    Treatment   Treatment
    ## 3545            Control      Grill         Main    Treatment   Treatment
    ## 3546            Control Quesadilla         Main    Satellite   Satellite
    ## 3547            Control      Grill Modification    Treatment   Treatment
    ## 3548            Control  Grab N Go         Side    Satellite   Satellite
    ## 3549            Control      Grill         Main    Treatment   Treatment
    ## 3550            Control      Grill         Main    Treatment   Treatment
    ## 3551            Control      Grill Modification    Treatment   Treatment
    ## 3552            Control      Grill Modification    Treatment   Treatment
    ## 3553            Control      Grill Modification    Treatment   Treatment
    ## 3554            Control        Wok         Main    Satellite   Satellite
    ## 3555            Control        Wok         Main    Satellite   Satellite
    ## 3556            Control      Ramen         Main    Treatment   Treatment
    ## 3557            Control        Wok         Main    Satellite   Satellite
    ## 3558            Control      Ramen         Main    Treatment   Treatment
    ## 3559            Control        Wok         Main    Satellite   Satellite
    ## 3560            Control        Wok         Side    Satellite   Satellite
    ## 3561            Control        Wok         Side    Satellite   Satellite
    ## 3562            Control  Breakfast         Main    Satellite   Satellite
    ## 3563            Control  Breakfast         Side    Satellite   Satellite
    ## 3564            Control  Breakfast         Main    Satellite   Satellite
    ## 3565            Control  Breakfast Modification    Satellite   Satellite
    ## 3566            Control  Breakfast         Side    Satellite   Satellite
    ## 3567            Control  Breakfast         Side    Satellite   Satellite
    ## 3568            Control  Breakfast         Side    Satellite   Satellite
    ## 3569            Control  Breakfast         Side    Satellite   Satellite
    ## 3570            Control  Breakfast         Side    Satellite   Satellite
    ## 3571            Control      Pasta         Main    Satellite   Satellite
    ## 3572            Control      Pizza         Main    Satellite   Satellite
    ## 3573            Control      Pasta         Main    Satellite   Satellite
    ## 3574            Control      Pizza         Main    Satellite   Satellite
    ## 3575            Control      Pasta Modification    Satellite   Satellite
    ## 3576            Control      Pasta         Side    Satellite   Satellite
    ## 3577            Control       Wrap         Main    Satellite   Satellite
    ## 3578            Control       Wrap         Main    Satellite   Satellite
    ## 3579            Control       Wrap         Side    Satellite   Satellite
    ## 3580            Control       Wrap Modification    Satellite   Satellite
    ## 3581            Control  Grab N Go         Main    Satellite   Satellite
    ## 3582            Control  Grab N Go         Main    Satellite   Satellite
    ## 3583            Control  Grab N Go         Main    Satellite   Satellite
    ## 3584            Control       Soup         Main    Satellite   Satellite
    ## 3585            Control       Soup         Main    Satellite   Satellite
    ## 3586            Control  Salad Bar         Main    Satellite   Satellite
    ## 3587            Control  Salad Bar Modification    Satellite   Satellite
    ## 3588            Control Quesadilla         Main    Satellite   Satellite
    ## 3589            Control      Grill         Main    Treatment   Treatment
    ## 3590            Control Quesadilla         Main    Satellite   Satellite
    ## 3591            Control  Grab N Go         Side    Satellite   Satellite
    ## 3592            Control      Grill         Side    Treatment   Treatment
    ## 3593            Control Quesadilla         Main    Satellite   Satellite
    ## 3594            Control      Grill         Main    Treatment   Treatment
    ## 3595            Control      Grill         Main    Treatment   Treatment
    ## 3596            Control      Grill         Side    Treatment   Treatment
    ## 3597            Control      Grill         Main    Treatment   Treatment
    ## 3598            Control      Grill Modification    Treatment   Treatment
    ## 3599            Control  Grab N Go         Side    Satellite   Satellite
    ## 3600            Control      Grill         Main    Treatment   Treatment
    ## 3601            Control      Grill Modification    Treatment   Treatment
    ## 3602            Control      Grill Modification    Treatment   Treatment
    ## 3603            Control      Grill Modification    Treatment   Treatment
    ## 3604            Control        Wok         Main    Satellite   Satellite
    ## 3605            Control      Ramen         Main    Treatment   Treatment
    ## 3606            Control        Wok         Main    Satellite   Satellite
    ## 3607            Control        Wok         Main    Satellite   Satellite
    ## 3608            Control      Ramen         Main    Treatment   Treatment
    ## 3609            Control        Wok         Main    Satellite   Satellite
    ## 3610            Control        Wok         Side    Satellite   Satellite
    ## 3611            Control        Wok         Side    Satellite   Satellite
    ## 3612            Control        Wok         Side    Satellite   Satellite
    ## 3613            Control        Wok         Side    Satellite   Satellite
    ## 3614            Control        Wok         Side    Satellite   Satellite
    ## 3615            Control        Wok         Side    Satellite   Satellite
    ## 3616            Control      Pasta         Main    Satellite   Satellite
    ## 3617            Control      Pizza         Main    Satellite   Satellite
    ## 3618            Control      Pasta         Main    Satellite   Satellite
    ## 3619            Control      Pizza         Main    Satellite   Satellite
    ## 3620            Control      Pasta Modification    Satellite   Satellite
    ## 3621            Control  Breakfast         Main    Satellite   Satellite
    ## 3622            Control  Breakfast         Side    Satellite   Satellite
    ## 3623            Control  Breakfast Modification    Satellite   Satellite
    ## 3624            Control  Breakfast         Main    Satellite   Satellite
    ## 3625            Control  Breakfast         Side    Satellite   Satellite
    ## 3626            Control  Breakfast         Side    Satellite   Satellite
    ## 3627            Control  Breakfast         Side    Satellite   Satellite
    ## 3628            Control       Wrap         Main    Satellite   Satellite
    ## 3629            Control       Wrap         Side    Satellite   Satellite
    ## 3630            Control       Wrap         Main    Satellite   Satellite
    ## 3631            Control  Grab N Go         Main    Satellite   Satellite
    ## 3632            Control  Grab N Go         Main    Satellite   Satellite
    ## 3633            Control  Grab N Go         Main    Satellite   Satellite
    ## 3634            Control  Salad Bar         Main    Satellite   Satellite
    ## 3635            Control  Salad Bar Modification    Satellite   Satellite
    ## 3636            Control       Soup         Main    Satellite   Satellite
    ## 3637            Control       Soup         Main    Satellite   Satellite
    ## 3638            Control Quesadilla         Main    Satellite   Satellite
    ## 3639            Control      Grill         Main    Treatment   Treatment
    ## 3640            Control  Grab N Go         Side    Satellite   Satellite
    ## 3641            Control Quesadilla         Main    Satellite   Satellite
    ## 3642            Control      Grill         Side    Treatment   Treatment
    ## 3643            Control      Grill         Main    Treatment   Treatment
    ## 3644            Control Quesadilla         Main    Satellite   Satellite
    ## 3645            Control      Grill         Side    Treatment   Treatment
    ## 3646            Control      Grill         Main    Treatment   Treatment
    ## 3647            Control      Grill         Main    Treatment   Treatment
    ## 3648            Control      Grill Modification    Treatment   Treatment
    ## 3649            Control  Grab N Go         Side    Satellite   Satellite
    ## 3650            Control      Grill Modification    Treatment   Treatment
    ## 3651            Control      Grill         Main    Treatment   Treatment
    ## 3652            Control      Grill Modification    Treatment   Treatment
    ## 3653            Control      Grill Modification    Treatment   Treatment
    ## 3654            Control      Grill Modification    Treatment   Treatment
    ## 3655            Control        Wok         Main    Satellite   Satellite
    ## 3656            Control      Ramen         Main    Treatment   Treatment
    ## 3657            Control        Wok         Main    Satellite   Satellite
    ## 3658            Control        Wok         Main    Satellite   Satellite
    ## 3659            Control      Ramen         Main    Treatment   Treatment
    ## 3660            Control        Wok         Main    Satellite   Satellite
    ## 3661            Control        Wok         Side    Satellite   Satellite
    ## 3662            Control        Wok         Side    Satellite   Satellite
    ## 3663            Control        Wok         Side    Satellite   Satellite
    ## 3664            Control        Wok         Side    Satellite   Satellite
    ## 3665            Control        Wok         Side    Satellite   Satellite
    ## 3666            Control  Breakfast         Main    Satellite   Satellite
    ## 3667            Control  Breakfast         Side    Satellite   Satellite
    ## 3668            Control  Breakfast         Main    Satellite   Satellite
    ## 3669            Control  Breakfast Modification    Satellite   Satellite
    ## 3670            Control  Breakfast         Side    Satellite   Satellite
    ## 3671            Control  Breakfast         Side    Satellite   Satellite
    ## 3672            Control  Breakfast         Side    Satellite   Satellite
    ## 3673            Control  Breakfast         Side    Satellite   Satellite
    ## 3674            Control  Breakfast         Side    Satellite   Satellite
    ## 3675            Control      Pasta         Main    Satellite   Satellite
    ## 3676            Control      Pasta         Main    Satellite   Satellite
    ## 3677            Control      Pizza         Main    Satellite   Satellite
    ## 3678            Control      Pizza         Main    Satellite   Satellite
    ## 3679            Control      Pasta Modification    Satellite   Satellite
    ## 3680            Control       Wrap         Main    Satellite   Satellite
    ## 3681            Control       Wrap         Main    Satellite   Satellite
    ## 3682            Control       Wrap         Side    Satellite   Satellite
    ## 3683            Control       Wrap Modification    Satellite   Satellite
    ## 3684            Control  Grab N Go         Main    Satellite   Satellite
    ## 3685            Control  Grab N Go         Main    Satellite   Satellite
    ## 3686            Control  Grab N Go         Main    Satellite   Satellite
    ## 3687            Control  Grab N Go         Main    Satellite   Satellite
    ## 3688            Control  Salad Bar         Main    Satellite   Satellite
    ## 3689            Control       Soup         Main    Satellite   Satellite
    ## 3690            Control       Soup         Main    Satellite   Satellite
    ## 3691            Control Quesadilla         Main    Satellite   Satellite
    ## 3692            Control      Grill         Main    Treatment   Treatment
    ## 3693            Control  Grab N Go         Side    Satellite   Satellite
    ## 3694            Control Quesadilla         Main    Satellite   Satellite
    ## 3695            Control      Grill         Side    Treatment   Treatment
    ## 3696            Control      Grill         Main    Treatment   Treatment
    ## 3697            Control      Grill Modification    Treatment   Treatment
    ## 3698            Control Quesadilla         Main    Satellite   Satellite
    ## 3699            Control      Grill         Side    Treatment   Treatment
    ## 3700            Control      Grill         Main    Treatment   Treatment
    ## 3701            Control  Grab N Go         Side    Satellite   Satellite
    ## 3702            Control      Grill         Main    Treatment   Treatment
    ## 3703            Control      Grill         Main    Treatment   Treatment
    ## 3704            Control      Grill Modification    Treatment   Treatment
    ## 3705            Control      Grill Modification    Treatment   Treatment
    ## 3706            Control      Grill Modification    Treatment   Treatment
    ## 3707            Control      Grill Modification    Treatment   Treatment
    ## 3708            Control        Wok         Main    Satellite   Satellite
    ## 3709            Control        Wok         Main    Satellite   Satellite
    ## 3710            Control      Ramen         Main    Treatment   Treatment
    ## 3711            Control        Wok         Main    Satellite   Satellite
    ## 3712            Control      Ramen         Main    Treatment   Treatment
    ## 3713            Control        Wok         Main    Satellite   Satellite
    ## 3714            Control        Wok         Side    Satellite   Satellite
    ## 3715            Control        Wok         Side    Satellite   Satellite
    ## 3716            Control      Ramen         Main    Treatment   Treatment
    ## 3717            Control        Wok         Side    Satellite   Satellite
    ## 3718            Control        Wok         Side    Satellite   Satellite
    ## 3719            Control        Wok         Side    Satellite   Satellite
    ## 3720            Control      Pasta         Main    Satellite   Satellite
    ## 3721            Control      Pasta         Main    Satellite   Satellite
    ## 3722            Control      Pizza         Main    Satellite   Satellite
    ## 3723            Control      Pasta Modification    Satellite   Satellite
    ## 3724            Control      Pizza         Main    Satellite   Satellite
    ## 3725            Control      Pasta         Side    Satellite   Satellite
    ## 3726            Control  Breakfast         Main    Satellite   Satellite
    ## 3727            Control  Breakfast         Side    Satellite   Satellite
    ## 3728            Control  Breakfast         Main    Satellite   Satellite
    ## 3729            Control  Breakfast Modification    Satellite   Satellite
    ## 3730            Control  Breakfast         Side    Satellite   Satellite
    ## 3731            Control  Breakfast         Side    Satellite   Satellite
    ## 3732            Control  Breakfast         Side    Satellite   Satellite
    ## 3733            Control  Breakfast         Side    Satellite   Satellite
    ## 3734            Control  Breakfast         Side    Satellite   Satellite
    ## 3735            Control  Breakfast         Side    Satellite   Satellite
    ## 3736            Control       Wrap         Main    Satellite   Satellite
    ## 3737            Control       Wrap         Main    Satellite   Satellite
    ## 3738            Control       Wrap         Side    Satellite   Satellite
    ## 3739            Control  Grab N Go         Main    Satellite   Satellite
    ## 3740            Control  Grab N Go         Main    Satellite   Satellite
    ## 3741            Control  Grab N Go         Main    Satellite   Satellite
    ## 3742            Control       Soup         Main    Satellite   Satellite
    ## 3743            Control       Soup         Main    Satellite   Satellite
    ## 3744            Control  Salad Bar         Main    Satellite   Satellite
    ## 3745            Control  Salad Bar Modification    Satellite   Satellite
    ## 3746            Control Quesadilla         Main    Satellite   Satellite
    ## 3747            Control      Grill         Main    Treatment   Treatment
    ## 3748            Control  Grab N Go         Side    Satellite   Satellite
    ## 3749            Control Quesadilla         Main    Satellite   Satellite
    ## 3750            Control      Grill         Side    Treatment   Treatment
    ## 3751            Control Quesadilla         Main    Satellite   Satellite
    ## 3752            Control      Grill         Side    Treatment   Treatment
    ## 3753            Control      Grill         Main    Treatment   Treatment
    ## 3754            Control      Grill         Main    Treatment   Treatment
    ## 3755            Control      Grill Modification    Treatment   Treatment
    ## 3756            Control  Grab N Go         Side    Satellite   Satellite
    ## 3757            Control      Grill         Main    Treatment   Treatment
    ## 3758            Control      Grill Modification    Treatment   Treatment
    ## 3759            Control      Grill         Main    Treatment   Treatment
    ## 3760            Control      Grill Modification    Treatment   Treatment
    ## 3761            Control      Grill Modification    Treatment   Treatment
    ## 3762            Control      Grill Modification    Treatment   Treatment
    ## 3763            Control        Wok         Main    Satellite   Satellite
    ## 3764            Control      Ramen         Main    Treatment   Treatment
    ## 3765            Control        Wok         Main    Satellite   Satellite
    ## 3766            Control        Wok         Main    Satellite   Satellite
    ## 3767            Control      Ramen         Main    Treatment   Treatment
    ## 3768            Control        Wok         Main    Satellite   Satellite
    ## 3769            Control        Wok         Side    Satellite   Satellite
    ## 3770            Control        Wok         Side    Satellite   Satellite
    ## 3771            Control      Ramen         Main    Treatment   Treatment
    ## 3772            Control        Wok         Side    Satellite   Satellite
    ## 3773            Control        Wok         Side    Satellite   Satellite
    ## 3774            Control      Pasta         Main    Satellite   Satellite
    ## 3775            Control      Pizza         Main    Satellite   Satellite
    ## 3776            Control      Pasta         Main    Satellite   Satellite
    ## 3777            Control      Pizza         Main    Satellite   Satellite
    ## 3778            Control      Pasta Modification    Satellite   Satellite
    ## 3779            Control  Breakfast         Main    Satellite   Satellite
    ## 3780            Control  Breakfast         Side    Satellite   Satellite
    ## 3781            Control  Breakfast         Main    Satellite   Satellite
    ## 3782            Control  Breakfast Modification    Satellite   Satellite
    ## 3783            Control  Breakfast         Side    Satellite   Satellite
    ## 3784            Control  Breakfast         Side    Satellite   Satellite
    ## 3785            Control  Breakfast         Side    Satellite   Satellite
    ## 3786            Control       Wrap         Main    Satellite   Satellite
    ## 3787            Control       Wrap         Main    Satellite   Satellite
    ## 3788            Control       Wrap         Side    Satellite   Satellite
    ## 3789            Control  Grab N Go         Main    Satellite   Satellite
    ## 3790            Control  Grab N Go         Main    Satellite   Satellite
    ## 3791            Control  Grab N Go         Main    Satellite   Satellite
    ## 3792            Control  Grab N Go         Main    Satellite   Satellite
    ## 3793            Control  Salad Bar         Main    Satellite   Satellite
    ## 3794            Control  Salad Bar Modification    Satellite   Satellite
    ## 3795            Control       Soup         Main    Satellite   Satellite
    ## 3796            Control       Soup         Main    Satellite   Satellite
    ## 3797            Control Quesadilla         Main    Satellite   Satellite
    ## 3798            Control      Grill         Main    Treatment   Treatment
    ## 3799            Control Quesadilla         Main    Satellite   Satellite
    ## 3800            Control  Grab N Go         Side    Satellite   Satellite
    ## 3801            Control      Grill         Side    Treatment   Treatment
    ## 3802            Control Quesadilla         Main    Satellite   Satellite
    ## 3803            Control      Grill Modification    Treatment   Treatment
    ## 3804            Control      Grill         Main    Treatment   Treatment
    ## 3805            Control      Grill         Main    Treatment   Treatment
    ## 3806            Control      Grill         Side    Treatment   Treatment
    ## 3807            Control      Grill         Main    Treatment   Treatment
    ## 3808            Control  Grab N Go         Side    Satellite   Satellite
    ## 3809            Control      Grill         Main    Treatment   Treatment
    ## 3810            Control      Grill Modification    Treatment   Treatment
    ## 3811            Control      Grill Modification    Treatment   Treatment
    ## 3812            Control      Grill Modification    Treatment   Treatment
    ## 3813            Control        Wok         Main    Satellite   Satellite
    ## 3814            Control      Ramen         Main    Treatment   Treatment
    ## 3815            Control        Wok         Main    Satellite   Satellite
    ## 3816            Control      Ramen         Main    Treatment   Treatment
    ## 3817            Control        Wok         Main    Satellite   Satellite
    ## 3818            Control        Wok         Main    Satellite   Satellite
    ## 3819            Control        Wok         Side    Satellite   Satellite
    ## 3820            Control        Wok         Side    Satellite   Satellite
    ## 3821            Control        Wok         Side    Satellite   Satellite
    ## 3822            Control        Wok         Side    Satellite   Satellite
    ## 3823            Control        Wok         Side    Satellite   Satellite
    ## 3824            Control  Breakfast         Main    Satellite   Satellite
    ## 3825            Control  Breakfast         Side    Satellite   Satellite
    ## 3826            Control  Breakfast         Main    Satellite   Satellite
    ## 3827            Control  Breakfast Modification    Satellite   Satellite
    ## 3828            Control  Breakfast         Side    Satellite   Satellite
    ## 3829            Control  Breakfast         Side    Satellite   Satellite
    ## 3830            Control  Breakfast         Side    Satellite   Satellite
    ## 3831            Control  Breakfast         Side    Satellite   Satellite
    ## 3832            Control  Breakfast         Side    Satellite   Satellite
    ## 3833            Control      Pasta         Main    Satellite   Satellite
    ## 3834            Control      Pasta         Main    Satellite   Satellite
    ## 3835            Control      Pizza         Main    Satellite   Satellite
    ## 3836            Control      Pizza         Main    Satellite   Satellite
    ## 3837            Control      Pasta Modification    Satellite   Satellite
    ## 3838            Control      Pasta         Side    Satellite   Satellite
    ## 3839            Control       Wrap         Main    Satellite   Satellite
    ## 3840            Control       Wrap         Side    Satellite   Satellite
    ## 3841            Control       Soup         Main    Satellite   Satellite
    ## 3842            Control       Soup         Main    Satellite   Satellite
    ## 3843            Control  Grab N Go         Main    Satellite   Satellite
    ## 3844            Control  Grab N Go         Main    Satellite   Satellite
    ## 3845            Control  Grab N Go         Main    Satellite   Satellite
    ## 3846            Control  Salad Bar         Main    Satellite   Satellite
    ## 3847           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3848           Unimodal      Grill         Main    Treatment   Treatment
    ## 3849           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3850           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3851           Unimodal      Grill         Side    Treatment   Treatment
    ## 3852           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3853           Unimodal      Grill         Main    Treatment   Treatment
    ## 3854           Unimodal      Grill         Side    Treatment   Treatment
    ## 3855           Unimodal      Grill Modification    Treatment   Treatment
    ## 3856           Unimodal      Grill         Main    Treatment   Treatment
    ## 3857           Unimodal      Grill         Main    Treatment   Treatment
    ## 3858           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3859           Unimodal      Grill         Main    Treatment   Treatment
    ## 3860           Unimodal      Grill Modification    Treatment   Treatment
    ## 3861           Unimodal      Grill Modification    Treatment   Treatment
    ## 3862           Unimodal      Grill Modification    Treatment   Treatment
    ## 3863           Unimodal      Grill Modification    Treatment   Treatment
    ## 3864           Unimodal        Wok         Main    Satellite   Satellite
    ## 3865           Unimodal        Wok         Main    Satellite   Satellite
    ## 3866           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3867           Unimodal        Wok         Main    Satellite   Satellite
    ## 3868           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3869           Unimodal        Wok         Side    Satellite   Satellite
    ## 3870           Unimodal        Wok         Main    Satellite   Satellite
    ## 3871           Unimodal        Wok         Side    Satellite   Satellite
    ## 3872           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3873           Unimodal        Wok         Side    Satellite   Satellite
    ## 3874           Unimodal        Wok         Side    Satellite   Satellite
    ## 3875           Unimodal        Wok         Side    Satellite   Satellite
    ## 3876           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3877           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3878           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3879           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3880           Unimodal      Pasta Modification    Satellite   Satellite
    ## 3881           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3882           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3883           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 3884           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3885           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3886           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3887           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3888           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3889           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3890           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3891           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3892           Unimodal       Wrap         Side    Satellite   Satellite
    ## 3893           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3894           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3895           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3896           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 3897           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 3898           Unimodal       Soup         Main    Satellite   Satellite
    ## 3899           Unimodal       Soup         Main    Satellite   Satellite
    ## 3900           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3901           Unimodal      Grill         Main    Treatment   Treatment
    ## 3902           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3903           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3904           Unimodal      Grill         Side    Treatment   Treatment
    ## 3905           Unimodal      Grill         Main    Treatment   Treatment
    ## 3906           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3907           Unimodal      Grill         Side    Treatment   Treatment
    ## 3908           Unimodal      Grill Modification    Treatment   Treatment
    ## 3909           Unimodal      Grill         Main    Treatment   Treatment
    ## 3910           Unimodal      Grill         Main    Treatment   Treatment
    ## 3911           Unimodal      Grill Modification    Treatment   Treatment
    ## 3912           Unimodal      Grill         Main    Treatment   Treatment
    ## 3913           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3914           Unimodal      Grill Modification    Treatment   Treatment
    ## 3915           Unimodal      Grill Modification    Treatment   Treatment
    ## 3916           Unimodal      Grill Modification    Treatment   Treatment
    ## 3917           Unimodal        Wok         Main    Satellite   Satellite
    ## 3918           Unimodal        Wok         Main    Satellite   Satellite
    ## 3919           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3920           Unimodal        Wok         Main    Satellite   Satellite
    ## 3921           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3922           Unimodal        Wok         Main    Satellite   Satellite
    ## 3923           Unimodal        Wok         Side    Satellite   Satellite
    ## 3924           Unimodal        Wok         Side    Satellite   Satellite
    ## 3925           Unimodal        Wok         Side    Satellite   Satellite
    ## 3926           Unimodal        Wok         Side    Satellite   Satellite
    ## 3927           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3928           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3929           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3930           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3931           Unimodal      Pasta Modification    Satellite   Satellite
    ## 3932           Unimodal      Pasta         Side    Satellite   Satellite
    ## 3933           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3934           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3935           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3936           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 3937           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3938           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3939           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3940           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3941           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3942           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3943           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3944           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3945           Unimodal       Wrap         Side    Satellite   Satellite
    ## 3946           Unimodal       Wrap         Side    Satellite   Satellite
    ## 3947           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3948           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3949           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3950           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 3951           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 3952           Unimodal       Soup         Main    Satellite   Satellite
    ## 3953           Unimodal       Soup         Main    Satellite   Satellite
    ## 3954           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3955           Unimodal      Grill         Main    Treatment   Treatment
    ## 3956           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3957           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3958           Unimodal      Grill         Side    Treatment   Treatment
    ## 3959           Unimodal      Grill         Main    Treatment   Treatment
    ## 3960           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 3961           Unimodal      Grill         Side    Treatment   Treatment
    ## 3962           Unimodal      Grill         Main    Treatment   Treatment
    ## 3963           Unimodal      Grill Modification    Treatment   Treatment
    ## 3964           Unimodal      Grill         Main    Treatment   Treatment
    ## 3965           Unimodal      Grill         Main    Treatment   Treatment
    ## 3966           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 3967           Unimodal      Grill Modification    Treatment   Treatment
    ## 3968           Unimodal      Grill Modification    Treatment   Treatment
    ## 3969           Unimodal      Grill Modification    Treatment   Treatment
    ## 3970           Unimodal      Grill Modification    Treatment   Treatment
    ## 3971           Unimodal        Wok         Main    Satellite   Satellite
    ## 3972           Unimodal        Wok         Main    Satellite   Satellite
    ## 3973           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3974           Unimodal        Wok         Main    Satellite   Satellite
    ## 3975           Unimodal      Ramen         Main    Treatment   Treatment
    ## 3976           Unimodal        Wok         Side    Satellite   Satellite
    ## 3977           Unimodal        Wok         Main    Satellite   Satellite
    ## 3978           Unimodal        Wok         Side    Satellite   Satellite
    ## 3979           Unimodal        Wok         Side    Satellite   Satellite
    ## 3980           Unimodal        Wok         Side    Satellite   Satellite
    ## 3981           Unimodal        Wok         Side    Satellite   Satellite
    ## 3982           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3983           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3984           Unimodal      Pasta         Main    Satellite   Satellite
    ## 3985           Unimodal      Pasta Modification    Satellite   Satellite
    ## 3986           Unimodal      Pizza         Main    Satellite   Satellite
    ## 3987           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3988           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3989           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 3990           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 3991           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3992           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3993           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3994           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3995           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 3996           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3997           Unimodal       Wrap         Main    Satellite   Satellite
    ## 3998           Unimodal       Wrap         Side    Satellite   Satellite
    ## 3999           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4000           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4001           Unimodal       Soup         Main    Satellite   Satellite
    ## 4002           Unimodal       Soup         Main    Satellite   Satellite
    ## 4003           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4004           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4005           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4006           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4007           Unimodal      Grill         Main    Treatment   Treatment
    ## 4008           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4009           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4010           Unimodal      Grill         Side    Treatment   Treatment
    ## 4011           Unimodal      Grill         Main    Treatment   Treatment
    ## 4012           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4013           Unimodal      Grill         Side    Treatment   Treatment
    ## 4014           Unimodal      Grill         Main    Treatment   Treatment
    ## 4015           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4016           Unimodal      Grill         Main    Treatment   Treatment
    ## 4017           Unimodal      Grill Modification    Treatment   Treatment
    ## 4018           Unimodal      Grill Modification    Treatment   Treatment
    ## 4019           Unimodal      Grill Modification    Treatment   Treatment
    ## 4020           Unimodal      Grill Modification    Treatment   Treatment
    ## 4021           Unimodal        Wok         Main    Satellite   Satellite
    ## 4022           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4023           Unimodal        Wok         Main    Satellite   Satellite
    ## 4024           Unimodal        Wok         Main    Satellite   Satellite
    ## 4025           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4026           Unimodal        Wok         Main    Satellite   Satellite
    ## 4027           Unimodal        Wok         Side    Satellite   Satellite
    ## 4028           Unimodal        Wok         Side    Satellite   Satellite
    ## 4029           Unimodal        Wok         Side    Satellite   Satellite
    ## 4030           Unimodal        Wok         Side    Satellite   Satellite
    ## 4031           Unimodal        Wok         Side    Satellite   Satellite
    ## 4032           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4033           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4034           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4035           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4036           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4037           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4038           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4039           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4040           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4041           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4042           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4043           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4044           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4045           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4046           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4047           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4048           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4049           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4050           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4051           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4052           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4053           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4054           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4055           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4056           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4057           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4058           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4059           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4060           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4061           Unimodal       Soup         Main    Satellite   Satellite
    ## 4062           Unimodal       Soup         Main    Satellite   Satellite
    ## 4063           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4064           Unimodal      Grill         Main    Treatment   Treatment
    ## 4065           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4066           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4067           Unimodal      Grill         Side    Treatment   Treatment
    ## 4068           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4069           Unimodal      Grill         Main    Treatment   Treatment
    ## 4070           Unimodal      Grill         Main    Treatment   Treatment
    ## 4071           Unimodal      Grill         Side    Treatment   Treatment
    ## 4072           Unimodal      Grill Modification    Treatment   Treatment
    ## 4073           Unimodal      Grill         Main    Treatment   Treatment
    ## 4074           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4075           Unimodal      Grill         Main    Treatment   Treatment
    ## 4076           Unimodal      Grill Modification    Treatment   Treatment
    ## 4077           Unimodal      Grill Modification    Treatment   Treatment
    ## 4078           Unimodal      Grill Modification    Treatment   Treatment
    ## 4079           Unimodal      Grill Modification    Treatment   Treatment
    ## 4080           Unimodal        Wok         Main    Satellite   Satellite
    ## 4081           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4082           Unimodal        Wok         Main    Satellite   Satellite
    ## 4083           Unimodal        Wok         Main    Satellite   Satellite
    ## 4084           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4085           Unimodal        Wok         Main    Satellite   Satellite
    ## 4086           Unimodal        Wok         Side    Satellite   Satellite
    ## 4087           Unimodal        Wok         Side    Satellite   Satellite
    ## 4088           Unimodal        Wok         Side    Satellite   Satellite
    ## 4089           Unimodal        Wok         Side    Satellite   Satellite
    ## 4090           Unimodal        Wok         Side    Satellite   Satellite
    ## 4091           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4092           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4093           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4094           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4095           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4096           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4097           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4098           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4099           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4100           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4101           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4102           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4103           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4104           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4105           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4106           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4107           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4108           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4109           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4110           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4111           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4112           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4113           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4114           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4115           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4116           Unimodal       Soup         Main    Satellite   Satellite
    ## 4117           Unimodal       Soup         Main    Satellite   Satellite
    ## 4118           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4119           Unimodal      Grill         Main    Treatment   Treatment
    ## 4120           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4121           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4122           Unimodal      Grill         Side    Treatment   Treatment
    ## 4123           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4124           Unimodal      Grill         Main    Treatment   Treatment
    ## 4125           Unimodal      Grill         Main    Treatment   Treatment
    ## 4126           Unimodal      Grill         Side    Treatment   Treatment
    ## 4127           Unimodal      Grill Modification    Treatment   Treatment
    ## 4128           Unimodal      Grill         Main    Treatment   Treatment
    ## 4129           Unimodal      Grill         Main    Treatment   Treatment
    ## 4130           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4131           Unimodal      Grill Modification    Treatment   Treatment
    ## 4132           Unimodal      Grill Modification    Treatment   Treatment
    ## 4133           Unimodal      Grill Modification    Treatment   Treatment
    ## 4134           Unimodal      Grill Modification    Treatment   Treatment
    ## 4135           Unimodal        Wok         Main    Satellite   Satellite
    ## 4136           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4137           Unimodal        Wok         Main    Satellite   Satellite
    ## 4138           Unimodal        Wok         Main    Satellite   Satellite
    ## 4139           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4140           Unimodal        Wok         Side    Satellite   Satellite
    ## 4141           Unimodal        Wok         Main    Satellite   Satellite
    ## 4142           Unimodal        Wok         Side    Satellite   Satellite
    ## 4143           Unimodal        Wok         Side    Satellite   Satellite
    ## 4144           Unimodal        Wok         Side    Satellite   Satellite
    ## 4145           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4146           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4147           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4148           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4149           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4150           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4151           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4152           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4153           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4154           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4155           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4156           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4157           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4158           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4159           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4160           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4161           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4162           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4163           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4164           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4165           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4166           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4167           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4168           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4169           Unimodal       Soup         Main    Satellite   Satellite
    ## 4170           Unimodal       Soup         Main    Satellite   Satellite
    ## 4171           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4172           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4173           Unimodal      Grill         Main    Treatment   Treatment
    ## 4174           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4175           Unimodal      Grill         Side    Treatment   Treatment
    ## 4176           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4177           Unimodal      Grill         Main    Treatment   Treatment
    ## 4178           Unimodal      Grill         Side    Treatment   Treatment
    ## 4179           Unimodal      Grill         Main    Treatment   Treatment
    ## 4180           Unimodal      Grill Modification    Treatment   Treatment
    ## 4181           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4182           Unimodal      Grill         Main    Treatment   Treatment
    ## 4183           Unimodal      Grill         Main    Treatment   Treatment
    ## 4184           Unimodal      Grill Modification    Treatment   Treatment
    ## 4185           Unimodal      Grill Modification    Treatment   Treatment
    ## 4186           Unimodal      Grill Modification    Treatment   Treatment
    ## 4187           Unimodal      Grill Modification    Treatment   Treatment
    ## 4188           Unimodal        Wok         Main    Satellite   Satellite
    ## 4189           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4190           Unimodal        Wok         Main    Satellite   Satellite
    ## 4191           Unimodal        Wok         Main    Satellite   Satellite
    ## 4192           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4193           Unimodal        Wok         Side    Satellite   Satellite
    ## 4194           Unimodal        Wok         Main    Satellite   Satellite
    ## 4195           Unimodal        Wok         Side    Satellite   Satellite
    ## 4196           Unimodal        Wok         Side    Satellite   Satellite
    ## 4197           Unimodal        Wok         Side    Satellite   Satellite
    ## 4198           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4199           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4200           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4201           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4202           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4203           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4204           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4205           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4206           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4207           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4208           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4209           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4210           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4211           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4212           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4213           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4214           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4215           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4216           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4217           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4218           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4219           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4220           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4221           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4222           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4223           Unimodal       Soup         Main    Satellite   Satellite
    ## 4224           Unimodal       Soup         Main    Satellite   Satellite
    ## 4225           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4226           Unimodal      Grill         Main    Treatment   Treatment
    ## 4227           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4228           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4229           Unimodal      Grill         Side    Treatment   Treatment
    ## 4230           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4231           Unimodal      Grill         Main    Treatment   Treatment
    ## 4232           Unimodal      Grill         Side    Treatment   Treatment
    ## 4233           Unimodal      Grill Modification    Treatment   Treatment
    ## 4234           Unimodal      Grill         Main    Treatment   Treatment
    ## 4235           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4236           Unimodal      Grill         Main    Treatment   Treatment
    ## 4237           Unimodal      Grill         Main    Treatment   Treatment
    ## 4238           Unimodal      Grill Modification    Treatment   Treatment
    ## 4239           Unimodal      Grill Modification    Treatment   Treatment
    ## 4240           Unimodal      Grill Modification    Treatment   Treatment
    ## 4241           Unimodal      Grill Modification    Treatment   Treatment
    ## 4242           Unimodal        Wok         Main    Satellite   Satellite
    ## 4243           Unimodal        Wok         Main    Satellite   Satellite
    ## 4244           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4245           Unimodal        Wok         Main    Satellite   Satellite
    ## 4246           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4247           Unimodal        Wok         Main    Satellite   Satellite
    ## 4248           Unimodal        Wok         Side    Satellite   Satellite
    ## 4249           Unimodal        Wok         Side    Satellite   Satellite
    ## 4250           Unimodal        Wok         Side    Satellite   Satellite
    ## 4251           Unimodal        Wok         Side    Satellite   Satellite
    ## 4252           Unimodal        Wok         Side    Satellite   Satellite
    ## 4253           Unimodal      Ramen Modification    Treatment   Treatment
    ## 4254           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4255           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4256           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4257           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4258           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4259           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4260           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4261           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4262           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4263           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4264           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4265           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4266           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4267           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4268           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4269           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4270           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4271           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4272           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4273           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4274           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4275           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4276           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4277           Unimodal       Soup         Main    Satellite   Satellite
    ## 4278           Unimodal       Soup         Main    Satellite   Satellite
    ## 4279           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4280           Unimodal      Grill         Main    Treatment   Treatment
    ## 4281           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4282           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4283           Unimodal      Grill         Side    Treatment   Treatment
    ## 4284           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4285           Unimodal      Grill         Main    Treatment   Treatment
    ## 4286           Unimodal      Grill         Main    Treatment   Treatment
    ## 4287           Unimodal      Grill         Side    Treatment   Treatment
    ## 4288           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4289           Unimodal      Grill Modification    Treatment   Treatment
    ## 4290           Unimodal      Grill         Main    Treatment   Treatment
    ## 4291           Unimodal      Grill         Main    Treatment   Treatment
    ## 4292           Unimodal      Grill Modification    Treatment   Treatment
    ## 4293           Unimodal      Grill Modification    Treatment   Treatment
    ## 4294           Unimodal      Grill Modification    Treatment   Treatment
    ## 4295           Unimodal      Grill Modification    Treatment   Treatment
    ## 4296           Unimodal        Wok         Main    Satellite   Satellite
    ## 4297           Unimodal        Wok         Main    Satellite   Satellite
    ## 4298           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4299           Unimodal        Wok         Main    Satellite   Satellite
    ## 4300           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4301           Unimodal        Wok         Main    Satellite   Satellite
    ## 4302           Unimodal        Wok         Side    Satellite   Satellite
    ## 4303           Unimodal        Wok         Side    Satellite   Satellite
    ## 4304           Unimodal        Wok         Side    Satellite   Satellite
    ## 4305           Unimodal        Wok         Side    Satellite   Satellite
    ## 4306           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4307           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4308           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4309           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4310           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4311           Unimodal      Pasta         Side    Satellite   Satellite
    ## 4312           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4313           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4314           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4315           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4316           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4317           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4318           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4319           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4320           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4321           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4322           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4323           Unimodal       Wrap Modification    Satellite   Satellite
    ## 4324           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4325           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4326           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4327           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4328           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4329           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4330           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4331           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4332           Unimodal       Soup         Main    Satellite   Satellite
    ## 4333           Unimodal       Soup         Main    Satellite   Satellite
    ## 4334           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4335           Unimodal      Grill         Main    Treatment   Treatment
    ## 4336           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4337           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4338           Unimodal      Grill         Side    Treatment   Treatment
    ## 4339           Unimodal      Grill         Side    Treatment   Treatment
    ## 4340           Unimodal Quesadilla         Main    Satellite   Satellite
    ## 4341           Unimodal      Grill         Main    Treatment   Treatment
    ## 4342           Unimodal      Grill         Main    Treatment   Treatment
    ## 4343           Unimodal      Grill         Main    Treatment   Treatment
    ## 4344           Unimodal  Grab N Go         Side    Satellite   Satellite
    ## 4345           Unimodal      Grill Modification    Treatment   Treatment
    ## 4346           Unimodal      Grill         Main    Treatment   Treatment
    ## 4347           Unimodal      Grill Modification    Treatment   Treatment
    ## 4348           Unimodal      Grill Modification    Treatment   Treatment
    ## 4349           Unimodal      Grill Modification    Treatment   Treatment
    ## 4350           Unimodal        Wok         Main    Satellite   Satellite
    ## 4351           Unimodal        Wok         Main    Satellite   Satellite
    ## 4352           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4353           Unimodal        Wok         Main    Satellite   Satellite
    ## 4354           Unimodal      Ramen         Main    Treatment   Treatment
    ## 4355           Unimodal        Wok         Main    Satellite   Satellite
    ## 4356           Unimodal        Wok         Side    Satellite   Satellite
    ## 4357           Unimodal        Wok         Side    Satellite   Satellite
    ## 4358           Unimodal        Wok         Side    Satellite   Satellite
    ## 4359           Unimodal        Wok         Side    Satellite   Satellite
    ## 4360           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4361           Unimodal      Pasta         Main    Satellite   Satellite
    ## 4362           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4363           Unimodal      Pizza         Main    Satellite   Satellite
    ## 4364           Unimodal      Pasta Modification    Satellite   Satellite
    ## 4365           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4366           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4367           Unimodal  Breakfast         Main    Satellite   Satellite
    ## 4368           Unimodal  Breakfast Modification    Satellite   Satellite
    ## 4369           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4370           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4371           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4372           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4373           Unimodal  Breakfast         Side    Satellite   Satellite
    ## 4374           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4375           Unimodal       Wrap         Main    Satellite   Satellite
    ## 4376           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4377           Unimodal       Wrap         Side    Satellite   Satellite
    ## 4378           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4379           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4380           Unimodal  Grab N Go         Main    Satellite   Satellite
    ## 4381           Unimodal       Soup         Main    Satellite   Satellite
    ## 4382           Unimodal       Soup         Main    Satellite   Satellite
    ## 4383           Unimodal  Salad Bar         Main    Satellite   Satellite
    ## 4384           Unimodal  Salad Bar Modification    Satellite   Satellite
    ## 4385            Control Quesadilla         Main    Satellite   Satellite
    ## 4386            Control      Grill         Main    Treatment   Treatment
    ## 4387            Control Quesadilla         Main    Satellite   Satellite
    ## 4388            Control  Grab N Go         Side    Satellite   Satellite
    ## 4389            Control      Grill         Side    Treatment   Treatment
    ## 4390            Control      Grill         Main    Treatment   Treatment
    ## 4391            Control      Grill         Main    Treatment   Treatment
    ## 4392            Control Quesadilla         Main    Satellite   Satellite
    ## 4393            Control      Grill         Main    Treatment   Treatment
    ## 4394            Control      Grill         Side    Treatment   Treatment
    ## 4395            Control      Grill Modification    Treatment   Treatment
    ## 4396            Control  Grab N Go         Side    Satellite   Satellite
    ## 4397            Control      Grill         Main    Treatment   Treatment
    ## 4398            Control      Grill Modification    Treatment   Treatment
    ## 4399            Control      Grill Modification    Treatment   Treatment
    ## 4400            Control      Grill Modification    Treatment   Treatment
    ## 4401            Control      Grill Modification    Treatment   Treatment
    ## 4402            Control        Wok         Main    Satellite   Satellite
    ## 4403            Control        Wok         Main    Satellite   Satellite
    ## 4404            Control      Ramen         Main    Treatment   Treatment
    ## 4405            Control        Wok         Main    Satellite   Satellite
    ## 4406            Control      Ramen         Main    Treatment   Treatment
    ## 4407            Control        Wok         Main    Satellite   Satellite
    ## 4408            Control        Wok         Side    Satellite   Satellite
    ## 4409            Control        Wok         Side    Satellite   Satellite
    ## 4410            Control        Wok         Side    Satellite   Satellite
    ## 4411            Control      Pasta         Main    Satellite   Satellite
    ## 4412            Control      Pizza         Main    Satellite   Satellite
    ## 4413            Control      Pasta         Main    Satellite   Satellite
    ## 4414            Control      Pizza         Main    Satellite   Satellite
    ## 4415            Control      Pasta Modification    Satellite   Satellite
    ## 4416            Control  Breakfast         Main    Satellite   Satellite
    ## 4417            Control  Breakfast         Side    Satellite   Satellite
    ## 4418            Control  Breakfast         Main    Satellite   Satellite
    ## 4419            Control  Breakfast Modification    Satellite   Satellite
    ## 4420            Control  Breakfast         Side    Satellite   Satellite
    ## 4421            Control  Breakfast         Side    Satellite   Satellite
    ## 4422            Control  Breakfast         Side    Satellite   Satellite
    ## 4423            Control  Breakfast         Side    Satellite   Satellite
    ## 4424            Control  Breakfast         Side    Satellite   Satellite
    ## 4425            Control  Breakfast         Side    Satellite   Satellite
    ## 4426            Control       Wrap         Main    Satellite   Satellite
    ## 4427            Control       Wrap         Main    Satellite   Satellite
    ## 4428            Control       Wrap         Side    Satellite   Satellite
    ## 4429            Control       Wrap         Side    Satellite   Satellite
    ## 4430            Control       Wrap         Side    Satellite   Satellite
    ## 4431            Control  Salad Bar         Main    Satellite   Satellite
    ## 4432            Control  Salad Bar Modification    Satellite   Satellite
    ## 4433            Control       Soup         Main    Satellite   Satellite
    ## 4434            Control       Soup         Main    Satellite   Satellite
    ## 4435            Control  Grab N Go         Main    Satellite   Satellite
    ## 4436            Control  Grab N Go         Main    Satellite   Satellite
    ## 4437            Control  Grab N Go         Main    Satellite   Satellite
    ## 4438            Control Quesadilla         Main    Satellite   Satellite
    ## 4439            Control      Grill         Main    Treatment   Treatment
    ## 4440            Control  Grab N Go         Side    Satellite   Satellite
    ## 4441            Control Quesadilla         Main    Satellite   Satellite
    ## 4442            Control      Grill         Side    Treatment   Treatment
    ## 4443            Control Quesadilla         Main    Satellite   Satellite
    ## 4444            Control      Grill         Main    Treatment   Treatment
    ## 4445            Control      Grill         Side    Treatment   Treatment
    ## 4446            Control      Grill         Main    Treatment   Treatment
    ## 4447            Control      Grill         Main    Treatment   Treatment
    ## 4448            Control      Grill         Main    Treatment   Treatment
    ## 4449            Control      Grill Modification    Treatment   Treatment
    ## 4450            Control  Grab N Go         Side    Satellite   Satellite
    ## 4451            Control      Grill Modification    Treatment   Treatment
    ## 4452            Control      Grill Modification    Treatment   Treatment
    ## 4453            Control      Grill Modification    Treatment   Treatment
    ## 4454            Control        Wok         Main    Satellite   Satellite
    ## 4455            Control      Ramen         Main    Treatment   Treatment
    ## 4456            Control        Wok         Main    Satellite   Satellite
    ## 4457            Control        Wok         Main    Satellite   Satellite
    ## 4458            Control      Ramen         Main    Treatment   Treatment
    ## 4459            Control        Wok         Side    Satellite   Satellite
    ## 4460            Control        Wok         Side    Satellite   Satellite
    ## 4461            Control        Wok         Side    Satellite   Satellite
    ## 4462            Control        Wok         Main    Satellite   Satellite
    ## 4463            Control        Wok         Side    Satellite   Satellite
    ## 4464            Control      Ramen         Main    Treatment   Treatment
    ## 4465            Control        Wok         Side    Satellite   Satellite
    ## 4466            Control        Wok         Side    Satellite   Satellite
    ## 4467            Control  Breakfast         Main    Satellite   Satellite
    ## 4468            Control  Breakfast         Side    Satellite   Satellite
    ## 4469            Control  Breakfast         Main    Satellite   Satellite
    ## 4470            Control  Breakfast Modification    Satellite   Satellite
    ## 4471            Control  Breakfast         Side    Satellite   Satellite
    ## 4472            Control  Breakfast         Side    Satellite   Satellite
    ## 4473            Control  Breakfast         Side    Satellite   Satellite
    ## 4474            Control  Breakfast         Side    Satellite   Satellite
    ## 4475            Control  Breakfast         Side    Satellite   Satellite
    ## 4476            Control  Breakfast         Side    Satellite   Satellite
    ## 4477            Control      Pasta         Main    Satellite   Satellite
    ## 4478            Control      Pasta         Main    Satellite   Satellite
    ## 4479            Control      Pizza         Main    Satellite   Satellite
    ## 4480            Control      Pizza         Main    Satellite   Satellite
    ## 4481            Control      Pasta Modification    Satellite   Satellite
    ## 4482            Control      Pasta         Side    Satellite   Satellite
    ## 4483            Control       Wrap         Main    Satellite   Satellite
    ## 4484            Control       Wrap         Main    Satellite   Satellite
    ## 4485            Control       Wrap         Side    Satellite   Satellite
    ## 4486            Control       Wrap         Side    Satellite   Satellite
    ## 4487            Control       Wrap         Side    Satellite   Satellite
    ## 4488            Control       Wrap Modification    Satellite   Satellite
    ## 4489            Control  Grab N Go         Main    Satellite   Satellite
    ## 4490            Control  Grab N Go         Main    Satellite   Satellite
    ## 4491            Control  Grab N Go         Main    Satellite   Satellite
    ## 4492            Control  Grab N Go         Main    Satellite   Satellite
    ## 4493            Control  Salad Bar         Main    Satellite   Satellite
    ## 4494            Control  Salad Bar Modification    Satellite   Satellite
    ## 4495            Control       Soup         Main    Satellite   Satellite
    ## 4496            Control       Soup         Main    Satellite   Satellite
    ## 4497            Control Quesadilla         Main    Satellite   Satellite
    ## 4498            Control      Grill         Main    Treatment   Treatment
    ## 4499            Control  Grab N Go         Side    Satellite   Satellite
    ## 4500            Control Quesadilla         Main    Satellite   Satellite
    ## 4501            Control      Grill         Side    Treatment   Treatment
    ## 4502            Control      Grill         Main    Treatment   Treatment
    ## 4503            Control Quesadilla         Main    Satellite   Satellite
    ## 4504            Control      Grill         Side    Treatment   Treatment
    ## 4505            Control      Grill         Main    Treatment   Treatment
    ## 4506            Control      Grill         Main    Treatment   Treatment
    ## 4507            Control      Grill         Main    Treatment   Treatment
    ## 4508            Control      Grill Modification    Treatment   Treatment
    ## 4509            Control  Grab N Go         Side    Satellite   Satellite
    ## 4510            Control      Grill Modification    Treatment   Treatment
    ## 4511            Control      Grill Modification    Treatment   Treatment
    ## 4512            Control      Grill Modification    Treatment   Treatment
    ## 4513            Control      Grill Modification    Treatment   Treatment
    ## 4514            Control        Wok         Main    Satellite   Satellite
    ## 4515            Control        Wok         Main    Satellite   Satellite
    ## 4516            Control      Ramen         Main    Treatment   Treatment
    ## 4517            Control        Wok         Main    Satellite   Satellite
    ## 4518            Control      Ramen         Main    Treatment   Treatment
    ## 4519            Control        Wok         Main    Satellite   Satellite
    ## 4520            Control        Wok         Side    Satellite   Satellite
    ## 4521            Control        Wok         Side    Satellite   Satellite
    ## 4522            Control        Wok         Side    Satellite   Satellite
    ## 4523            Control        Wok         Side    Satellite   Satellite
    ## 4524            Control        Wok         Side    Satellite   Satellite
    ## 4525            Control      Pasta         Main    Satellite   Satellite
    ## 4526            Control      Pizza         Main    Satellite   Satellite
    ## 4527            Control      Pasta         Main    Satellite   Satellite
    ## 4528            Control      Pasta Modification    Satellite   Satellite
    ## 4529            Control      Pizza         Main    Satellite   Satellite
    ## 4530            Control      Pasta         Side    Satellite   Satellite
    ## 4531            Control  Breakfast         Main    Satellite   Satellite
    ## 4532            Control  Breakfast         Side    Satellite   Satellite
    ## 4533            Control  Breakfast         Main    Satellite   Satellite
    ## 4534            Control  Breakfast Modification    Satellite   Satellite
    ## 4535            Control  Breakfast         Side    Satellite   Satellite
    ## 4536            Control  Breakfast         Side    Satellite   Satellite
    ## 4537            Control  Breakfast         Side    Satellite   Satellite
    ## 4538            Control  Breakfast         Side    Satellite   Satellite
    ## 4539            Control  Breakfast         Side    Satellite   Satellite
    ## 4540            Control  Breakfast         Side    Satellite   Satellite
    ## 4541            Control       Wrap         Main    Satellite   Satellite
    ## 4542            Control       Wrap         Side    Satellite   Satellite
    ## 4543            Control       Wrap         Side    Satellite   Satellite
    ## 4544            Control       Soup         Main    Satellite   Satellite
    ## 4545            Control       Soup         Main    Satellite   Satellite
    ## 4546            Control  Grab N Go         Main    Satellite   Satellite
    ## 4547            Control  Grab N Go         Main    Satellite   Satellite
    ## 4548            Control  Grab N Go         Main    Satellite   Satellite
    ## 4549            Control  Salad Bar         Main    Satellite   Satellite
    ## 4550            Control  Salad Bar Modification    Satellite   Satellite
    ## 4551            Control Quesadilla         Main    Satellite   Satellite
    ## 4552            Control      Grill         Main    Treatment   Treatment
    ## 4553            Control  Grab N Go         Side    Satellite   Satellite
    ## 4554            Control Quesadilla         Main    Satellite   Satellite
    ## 4555            Control      Grill         Side    Treatment   Treatment
    ## 4556            Control Quesadilla         Main    Satellite   Satellite
    ## 4557            Control      Grill         Side    Treatment   Treatment
    ## 4558            Control      Grill         Main    Treatment   Treatment
    ## 4559            Control      Grill         Main    Treatment   Treatment
    ## 4560            Control      Grill         Main    Treatment   Treatment
    ## 4561            Control      Grill Modification    Treatment   Treatment
    ## 4562            Control  Grab N Go         Side    Satellite   Satellite
    ## 4563            Control      Grill         Main    Treatment   Treatment
    ## 4564            Control      Grill Modification    Treatment   Treatment
    ## 4565            Control      Grill Modification    Treatment   Treatment
    ## 4566            Control      Grill Modification    Treatment   Treatment
    ## 4567            Control      Grill Modification    Treatment   Treatment
    ## 4568            Control        Wok         Main    Satellite   Satellite
    ## 4569            Control        Wok         Main    Satellite   Satellite
    ## 4570            Control      Ramen         Main    Treatment   Treatment
    ## 4571            Control        Wok         Main    Satellite   Satellite
    ## 4572            Control      Ramen         Main    Treatment   Treatment
    ## 4573            Control        Wok         Side    Satellite   Satellite
    ## 4574            Control        Wok         Main    Satellite   Satellite
    ## 4575            Control        Wok         Side    Satellite   Satellite
    ## 4576            Control        Wok         Side    Satellite   Satellite
    ## 4577            Control        Wok         Side    Satellite   Satellite
    ## 4578            Control        Wok         Side    Satellite   Satellite
    ## 4579            Control  Breakfast         Main    Satellite   Satellite
    ## 4580            Control  Breakfast         Side    Satellite   Satellite
    ## 4581            Control  Breakfast         Main    Satellite   Satellite
    ## 4582            Control  Breakfast Modification    Satellite   Satellite
    ## 4583            Control  Breakfast         Side    Satellite   Satellite
    ## 4584            Control  Breakfast         Side    Satellite   Satellite
    ## 4585            Control  Breakfast         Side    Satellite   Satellite
    ## 4586            Control  Breakfast         Side    Satellite   Satellite
    ## 4587            Control  Breakfast         Side    Satellite   Satellite
    ## 4588            Control      Pasta         Main    Satellite   Satellite
    ## 4589            Control      Pasta         Main    Satellite   Satellite
    ## 4590            Control      Pizza         Main    Satellite   Satellite
    ## 4591            Control      Pizza         Main    Satellite   Satellite
    ## 4592            Control      Pasta Modification    Satellite   Satellite
    ## 4593            Control      Pasta         Side    Satellite   Satellite
    ## 4594            Control       Wrap         Main    Satellite   Satellite
    ## 4595            Control       Wrap         Main    Satellite   Satellite
    ## 4596            Control       Wrap         Side    Satellite   Satellite
    ## 4597            Control       Wrap         Side    Satellite   Satellite
    ## 4598            Control       Wrap         Side    Satellite   Satellite
    ## 4599            Control  Grab N Go         Main    Satellite   Satellite
    ## 4600            Control  Grab N Go         Main    Satellite   Satellite
    ## 4601            Control  Grab N Go         Main    Satellite   Satellite
    ## 4602            Control  Grab N Go         Main    Satellite   Satellite
    ## 4603            Control       Soup         Main    Satellite   Satellite
    ## 4604            Control       Soup         Main    Satellite   Satellite
    ## 4605            Control  Salad Bar         Main    Satellite   Satellite
    ## 4606            Control  Salad Bar Modification    Satellite   Satellite
    ## 4607            Control Quesadilla         Main    Satellite   Satellite
    ## 4608            Control      Grill         Main    Treatment   Treatment
    ## 4609            Control Quesadilla         Main    Satellite   Satellite
    ## 4610            Control  Grab N Go         Side    Satellite   Satellite
    ## 4611            Control      Grill         Side    Treatment   Treatment
    ## 4612            Control Quesadilla         Main    Satellite   Satellite
    ## 4613            Control      Grill         Main    Treatment   Treatment
    ## 4614            Control      Grill         Main    Treatment   Treatment
    ## 4615            Control      Grill         Main    Treatment   Treatment
    ## 4616            Control      Grill         Side    Treatment   Treatment
    ## 4617            Control      Grill         Main    Treatment   Treatment
    ## 4618            Control  Grab N Go         Side    Satellite   Satellite
    ## 4619            Control      Grill Modification    Treatment   Treatment
    ## 4620            Control      Grill Modification    Treatment   Treatment
    ## 4621            Control      Grill Modification    Treatment   Treatment
    ## 4622            Control      Grill Modification    Treatment   Treatment
    ## 4623            Control        Wok         Main    Satellite   Satellite
    ## 4624            Control      Ramen         Main    Treatment   Treatment
    ## 4625            Control        Wok         Main    Satellite   Satellite
    ## 4626            Control        Wok         Main    Satellite   Satellite
    ## 4627            Control      Ramen         Main    Treatment   Treatment
    ## 4628            Control      Ramen         Main    Treatment   Treatment
    ## 4629            Control        Wok         Side    Satellite   Satellite
    ## 4630            Control        Wok         Main    Satellite   Satellite
    ## 4631            Control        Wok         Side    Satellite   Satellite
    ## 4632            Control        Wok         Side    Satellite   Satellite
    ## 4633            Control  Breakfast         Main    Satellite   Satellite
    ## 4634            Control  Breakfast         Side    Satellite   Satellite
    ## 4635            Control  Breakfast         Main    Satellite   Satellite
    ## 4636            Control  Breakfast Modification    Satellite   Satellite
    ## 4637            Control  Breakfast         Side    Satellite   Satellite
    ## 4638            Control  Breakfast         Side    Satellite   Satellite
    ## 4639            Control  Breakfast         Side    Satellite   Satellite
    ## 4640            Control  Breakfast         Side    Satellite   Satellite
    ## 4641            Control      Pasta         Main    Satellite   Satellite
    ## 4642            Control      Pizza         Main    Satellite   Satellite
    ## 4643            Control      Pasta         Main    Satellite   Satellite
    ## 4644            Control      Pizza         Main    Satellite   Satellite
    ## 4645            Control      Pasta Modification    Satellite   Satellite
    ## 4646            Control      Pasta         Side    Satellite   Satellite
    ## 4647            Control       Wrap         Main    Satellite   Satellite
    ## 4648            Control       Wrap         Main    Satellite   Satellite
    ## 4649            Control       Soup         Main    Satellite   Satellite
    ## 4650            Control       Soup         Main    Satellite   Satellite
    ## 4651            Control  Grab N Go         Main    Satellite   Satellite
    ## 4652            Control  Grab N Go         Main    Satellite   Satellite
    ## 4653            Control  Salad Bar         Main    Satellite   Satellite
    ## 4654            Control  Salad Bar Modification    Satellite   Satellite
    ## 4655            Control Quesadilla         Main    Satellite   Satellite
    ## 4656            Control      Grill         Main    Treatment   Treatment
    ## 4657            Control  Grab N Go         Side    Satellite   Satellite
    ## 4658            Control Quesadilla         Main    Satellite   Satellite
    ## 4659            Control      Grill         Side    Treatment   Treatment
    ## 4660            Control Quesadilla         Main    Satellite   Satellite
    ## 4661            Control      Grill         Main    Treatment   Treatment
    ## 4662            Control      Grill         Side    Treatment   Treatment
    ## 4663            Control      Grill Modification    Treatment   Treatment
    ## 4664            Control      Grill         Main    Treatment   Treatment
    ## 4665            Control      Grill         Main    Treatment   Treatment
    ## 4666            Control      Grill         Main    Treatment   Treatment
    ## 4667            Control  Grab N Go         Side    Satellite   Satellite
    ## 4668            Control      Grill Modification    Treatment   Treatment
    ## 4669            Control      Grill Modification    Treatment   Treatment
    ## 4670            Control      Grill Modification    Treatment   Treatment
    ## 4671            Control      Grill Modification    Treatment   Treatment
    ## 4672            Control        Wok         Main    Satellite   Satellite
    ## 4673            Control        Wok         Main    Satellite   Satellite
    ## 4674            Control      Ramen         Main    Treatment   Treatment
    ## 4675            Control        Wok         Main    Satellite   Satellite
    ## 4676            Control      Ramen         Main    Treatment   Treatment
    ## 4677            Control        Wok         Main    Satellite   Satellite
    ## 4678            Control        Wok         Side    Satellite   Satellite
    ## 4679            Control        Wok         Side    Satellite   Satellite
    ## 4680            Control      Ramen         Main    Treatment   Treatment
    ## 4681            Control        Wok         Side    Satellite   Satellite
    ## 4682            Control      Pasta         Main    Satellite   Satellite
    ## 4683            Control      Pasta         Main    Satellite   Satellite
    ## 4684            Control      Pizza         Main    Satellite   Satellite
    ## 4685            Control      Pasta Modification    Satellite   Satellite
    ## 4686            Control      Pizza         Main    Satellite   Satellite
    ## 4687            Control  Breakfast         Main    Satellite   Satellite
    ## 4688            Control  Breakfast         Side    Satellite   Satellite
    ## 4689            Control  Breakfast         Main    Satellite   Satellite
    ## 4690            Control  Breakfast Modification    Satellite   Satellite
    ## 4691            Control  Breakfast         Side    Satellite   Satellite
    ## 4692            Control  Breakfast         Side    Satellite   Satellite
    ## 4693            Control  Breakfast         Side    Satellite   Satellite
    ## 4694            Control  Breakfast         Side    Satellite   Satellite
    ## 4695            Control       Wrap         Main    Satellite   Satellite
    ## 4696            Control       Wrap         Side    Satellite   Satellite
    ## 4697            Control       Wrap         Main    Satellite   Satellite
    ## 4698            Control       Wrap         Side    Satellite   Satellite
    ## 4699            Control       Wrap         Side    Satellite   Satellite
    ## 4700            Control  Salad Bar         Main    Satellite   Satellite
    ## 4701            Control  Salad Bar Modification    Satellite   Satellite
    ## 4702            Control  Grab N Go         Main    Satellite   Satellite
    ## 4703            Control  Grab N Go         Main    Satellite   Satellite
    ## 4704            Control  Grab N Go         Main    Satellite   Satellite
    ## 4705            Control       Soup         Main    Satellite   Satellite
    ## 4706            Control       Soup         Main    Satellite   Satellite
    ## 4707            Control Quesadilla         Main    Satellite   Satellite
    ## 4708            Control      Grill         Main    Treatment   Treatment
    ## 4709            Control  Grab N Go         Side    Satellite   Satellite
    ## 4710            Control Quesadilla         Main    Satellite   Satellite
    ## 4711            Control      Grill         Side    Treatment   Treatment
    ## 4712            Control      Grill         Main    Treatment   Treatment
    ## 4713            Control Quesadilla         Main    Satellite   Satellite
    ## 4714            Control      Grill         Main    Treatment   Treatment
    ## 4715            Control      Grill         Side    Treatment   Treatment
    ## 4716            Control      Grill         Main    Treatment   Treatment
    ## 4717            Control      Grill Modification    Treatment   Treatment
    ## 4718            Control  Grab N Go         Side    Satellite   Satellite
    ## 4719            Control      Grill         Main    Treatment   Treatment
    ## 4720            Control      Grill Modification    Treatment   Treatment
    ## 4721            Control      Grill Modification    Treatment   Treatment
    ## 4722            Control      Grill Modification    Treatment   Treatment
    ## 4723            Control      Grill Modification    Treatment   Treatment
    ## 4724            Control        Wok         Main    Satellite   Satellite
    ## 4725            Control      Ramen         Main    Treatment   Treatment
    ## 4726            Control        Wok         Main    Satellite   Satellite
    ## 4727            Control        Wok         Main    Satellite   Satellite
    ## 4728            Control      Ramen         Main    Treatment   Treatment
    ## 4729            Control        Wok         Main    Satellite   Satellite
    ## 4730            Control        Wok         Side    Satellite   Satellite
    ## 4731            Control        Wok         Side    Satellite   Satellite
    ## 4732            Control        Wok         Side    Satellite   Satellite
    ## 4733            Control        Wok         Side    Satellite   Satellite
    ## 4734            Control        Wok         Side    Satellite   Satellite
    ## 4735            Control  Breakfast         Main    Satellite   Satellite
    ## 4736            Control  Breakfast         Side    Satellite   Satellite
    ## 4737            Control  Breakfast         Main    Satellite   Satellite
    ## 4738            Control  Breakfast Modification    Satellite   Satellite
    ## 4739            Control  Breakfast         Side    Satellite   Satellite
    ## 4740            Control  Breakfast         Side    Satellite   Satellite
    ## 4741            Control  Breakfast         Side    Satellite   Satellite
    ## 4742            Control  Breakfast         Side    Satellite   Satellite
    ## 4743            Control  Breakfast         Side    Satellite   Satellite
    ## 4744            Control  Breakfast         Side    Satellite   Satellite
    ## 4745            Control  Breakfast         Side    Satellite   Satellite
    ## 4746            Control  Breakfast         Side    Satellite   Satellite
    ## 4747            Control      Pasta         Main    Satellite   Satellite
    ## 4748            Control      Pasta         Main    Satellite   Satellite
    ## 4749            Control      Pizza         Main    Satellite   Satellite
    ## 4750            Control      Pasta Modification    Satellite   Satellite
    ## 4751            Control      Pizza         Main    Satellite   Satellite
    ## 4752            Control      Pasta         Side    Satellite   Satellite
    ## 4753            Control       Wrap         Main    Satellite   Satellite
    ## 4754            Control       Wrap         Main    Satellite   Satellite
    ## 4755            Control       Wrap         Side    Satellite   Satellite
    ## 4756            Control       Wrap         Side    Satellite   Satellite
    ## 4757            Control       Wrap Modification    Satellite   Satellite
    ## 4758            Control  Grab N Go         Main    Satellite   Satellite
    ## 4759            Control  Grab N Go         Main    Satellite   Satellite
    ## 4760            Control  Grab N Go         Main    Satellite   Satellite
    ## 4761            Control  Grab N Go         Main    Satellite   Satellite
    ## 4762            Control  Salad Bar         Main    Satellite   Satellite
    ## 4763            Control  Salad Bar Modification    Satellite   Satellite
    ## 4764            Control       Soup         Main    Satellite   Satellite
    ## 4765            Control       Soup         Main    Satellite   Satellite
    ## 4766            Control       Deli         Main    Satellite   Satellite
    ## 4767            Control Quesadilla         Main    Satellite   Satellite
    ## 4768            Control      Grill         Main    Treatment   Treatment
    ## 4769            Control  Grab N Go         Side    Satellite   Satellite
    ## 4770            Control Quesadilla         Main    Satellite   Satellite
    ## 4771            Control      Grill         Side    Treatment   Treatment
    ## 4772            Control      Grill         Main    Treatment   Treatment
    ## 4773            Control Quesadilla         Main    Satellite   Satellite
    ## 4774            Control      Grill         Side    Treatment   Treatment
    ## 4775            Control      Grill         Main    Treatment   Treatment
    ## 4776            Control      Grill         Main    Treatment   Treatment
    ## 4777            Control      Grill Modification    Treatment   Treatment
    ## 4778            Control  Grab N Go         Side    Satellite   Satellite
    ## 4779            Control      Grill Modification    Treatment   Treatment
    ## 4780            Control      Grill         Main    Treatment   Treatment
    ## 4781            Control      Grill Modification    Treatment   Treatment
    ## 4782            Control      Grill Modification    Treatment   Treatment
    ## 4783            Control        Wok         Main    Satellite   Satellite
    ## 4784            Control        Wok         Main    Satellite   Satellite
    ## 4785            Control      Ramen         Main    Treatment   Treatment
    ## 4786            Control        Wok         Main    Satellite   Satellite
    ## 4787            Control      Ramen         Main    Treatment   Treatment
    ## 4788            Control        Wok         Main    Satellite   Satellite
    ## 4789            Control        Wok         Side    Satellite   Satellite
    ## 4790            Control        Wok         Side    Satellite   Satellite
    ## 4791            Control        Wok         Side    Satellite   Satellite
    ## 4792            Control        Wok         Side    Satellite   Satellite
    ## 4793            Control        Wok         Side    Satellite   Satellite
    ## 4794            Control        Wok         Side    Satellite   Satellite
    ## 4795            Control      Pasta         Main    Satellite   Satellite
    ## 4796            Control      Pasta         Main    Satellite   Satellite
    ## 4797            Control      Pizza         Main    Satellite   Satellite
    ## 4798            Control      Pizza         Main    Satellite   Satellite
    ## 4799            Control      Pasta Modification    Satellite   Satellite
    ## 4800            Control  Breakfast         Main    Satellite   Satellite
    ## 4801            Control  Breakfast         Side    Satellite   Satellite
    ## 4802            Control  Breakfast         Main    Satellite   Satellite
    ## 4803            Control  Breakfast Modification    Satellite   Satellite
    ## 4804            Control  Breakfast         Side    Satellite   Satellite
    ## 4805            Control  Breakfast         Side    Satellite   Satellite
    ## 4806            Control  Breakfast         Side    Satellite   Satellite
    ## 4807            Control  Breakfast         Side    Satellite   Satellite
    ## 4808            Control       Wrap         Main    Satellite   Satellite
    ## 4809            Control       Wrap         Side    Satellite   Satellite
    ## 4810            Control       Wrap         Side    Satellite   Satellite
    ## 4811            Control       Wrap         Side    Satellite   Satellite
    ## 4812            Control  Salad Bar         Main    Satellite   Satellite
    ## 4813            Control  Salad Bar Modification    Satellite   Satellite
    ## 4814            Control       Soup         Main    Satellite   Satellite
    ## 4815            Control       Soup         Main    Satellite   Satellite
    ## 4816            Control  Grab N Go         Main    Satellite   Satellite
    ## 4817            Control  Grab N Go         Main    Satellite   Satellite
    ## 4818            Control  Grab N Go         Main    Satellite   Satellite
    ## 4819            Control Quesadilla         Main    Satellite   Satellite
    ## 4820            Control      Grill         Main    Treatment   Treatment
    ## 4821            Control  Grab N Go         Side    Satellite   Satellite
    ## 4822            Control Quesadilla         Main    Satellite   Satellite
    ## 4823            Control      Grill         Side    Treatment   Treatment
    ## 4824            Control      Grill         Main    Treatment   Treatment
    ## 4825            Control      Grill         Side    Treatment   Treatment
    ## 4826            Control Quesadilla         Main    Satellite   Satellite
    ## 4827            Control      Grill         Main    Treatment   Treatment
    ## 4828            Control      Grill         Main    Treatment   Treatment
    ## 4829            Control      Grill         Main    Treatment   Treatment
    ## 4830            Control      Grill Modification    Treatment   Treatment
    ## 4831            Control  Grab N Go         Side    Satellite   Satellite
    ## 4832            Control      Grill Modification    Treatment   Treatment
    ## 4833            Control      Grill Modification    Treatment   Treatment
    ## 4834            Control      Grill Modification    Treatment   Treatment
    ## 4835            Control      Grill Modification    Treatment   Treatment
    ## 4836            Control        Wok         Main    Satellite   Satellite
    ## 4837            Control      Ramen         Main    Treatment   Treatment
    ## 4838            Control        Wok         Main    Satellite   Satellite
    ## 4839            Control        Wok         Main    Satellite   Satellite
    ## 4840            Control      Ramen         Main    Treatment   Treatment
    ## 4841            Control        Wok         Side    Satellite   Satellite
    ## 4842            Control        Wok         Main    Satellite   Satellite
    ## 4843            Control        Wok         Side    Satellite   Satellite
    ## 4844            Control        Wok         Side    Satellite   Satellite
    ## 4845            Control        Wok         Side    Satellite   Satellite
    ## 4846            Control        Wok         Side    Satellite   Satellite
    ## 4847            Control      Ramen         Main    Treatment   Treatment
    ## 4848            Control  Breakfast         Main    Satellite   Satellite
    ## 4849            Control  Breakfast         Side    Satellite   Satellite
    ## 4850            Control  Breakfast         Main    Satellite   Satellite
    ## 4851            Control  Breakfast Modification    Satellite   Satellite
    ## 4852            Control  Breakfast         Side    Satellite   Satellite
    ## 4853            Control  Breakfast         Side    Satellite   Satellite
    ## 4854            Control  Breakfast         Side    Satellite   Satellite
    ## 4855            Control      Pasta         Main    Satellite   Satellite
    ## 4856            Control      Pasta         Main    Satellite   Satellite
    ## 4857            Control      Pizza         Main    Satellite   Satellite
    ## 4858            Control      Pizza         Main    Satellite   Satellite
    ## 4859            Control      Pasta Modification    Satellite   Satellite
    ## 4860            Control      Pasta         Side    Satellite   Satellite
    ## 4861            Control       Wrap         Main    Satellite   Satellite
    ## 4862            Control       Wrap         Main    Satellite   Satellite
    ## 4863            Control       Wrap         Side    Satellite   Satellite
    ## 4864            Control       Wrap         Side    Satellite   Satellite
    ## 4865            Control       Wrap         Side    Satellite   Satellite
    ## 4866            Control       Wrap Modification    Satellite   Satellite
    ## 4867            Control  Grab N Go         Main    Satellite   Satellite
    ## 4868            Control  Grab N Go         Main    Satellite   Satellite
    ## 4869            Control  Grab N Go         Main    Satellite   Satellite
    ## 4870            Control  Grab N Go         Main    Satellite   Satellite
    ## 4871            Control       Soup         Main    Satellite   Satellite
    ## 4872            Control       Soup         Main    Satellite   Satellite
    ## 4873            Control  Salad Bar         Main    Satellite   Satellite
    ## 4874            Control  Salad Bar Modification    Satellite   Satellite
    ## 4875            Control Quesadilla         Main    Satellite   Satellite
    ## 4876            Control      Grill         Main    Treatment   Treatment
    ## 4877            Control Quesadilla         Main    Satellite   Satellite
    ## 4878            Control  Grab N Go         Side    Satellite   Satellite
    ## 4879            Control      Grill         Side    Treatment   Treatment
    ## 4880            Control Quesadilla         Main    Satellite   Satellite
    ## 4881            Control      Grill         Main    Treatment   Treatment
    ## 4882            Control      Grill         Main    Treatment   Treatment
    ## 4883            Control      Grill         Main    Treatment   Treatment
    ## 4884            Control      Grill         Side    Treatment   Treatment
    ## 4885            Control  Grab N Go         Side    Satellite   Satellite
    ## 4886            Control      Grill         Main    Treatment   Treatment
    ## 4887            Control      Grill Modification    Treatment   Treatment
    ## 4888            Control      Grill Modification    Treatment   Treatment
    ## 4889            Control      Grill Modification    Treatment   Treatment
    ## 4890            Control      Grill Modification    Treatment   Treatment
    ## 4891            Control        Wok         Main    Satellite   Satellite
    ## 4892            Control      Ramen         Main    Treatment   Treatment
    ## 4893            Control        Wok         Main    Satellite   Satellite
    ## 4894            Control        Wok         Main    Satellite   Satellite
    ## 4895            Control      Ramen         Main    Treatment   Treatment
    ## 4896            Control        Wok         Main    Satellite   Satellite
    ## 4897            Control        Wok         Side    Satellite   Satellite
    ## 4898            Control        Wok         Side    Satellite   Satellite
    ## 4899            Control        Wok         Side    Satellite   Satellite
    ## 4900            Control        Wok         Side    Satellite   Satellite
    ## 4901            Control  Breakfast         Main    Satellite   Satellite
    ## 4902            Control  Breakfast         Side    Satellite   Satellite
    ## 4903            Control  Breakfast         Main    Satellite   Satellite
    ## 4904            Control  Breakfast Modification    Satellite   Satellite
    ## 4905            Control  Breakfast         Side    Satellite   Satellite
    ## 4906            Control  Breakfast         Side    Satellite   Satellite
    ## 4907            Control  Breakfast         Side    Satellite   Satellite
    ## 4908            Control  Breakfast         Side    Satellite   Satellite
    ## 4909            Control      Pasta         Main    Satellite   Satellite
    ## 4910            Control      Pizza         Main    Satellite   Satellite
    ## 4911            Control      Pasta         Main    Satellite   Satellite
    ## 4912            Control      Pizza         Main    Satellite   Satellite
    ## 4913            Control      Pasta Modification    Satellite   Satellite
    ## 4914            Control      Pasta         Side    Satellite   Satellite
    ## 4915            Control       Wrap         Main    Satellite   Satellite
    ## 4916            Control       Wrap         Side    Satellite   Satellite
    ## 4917            Control       Wrap         Side    Satellite   Satellite
    ## 4918            Control  Grab N Go         Main    Satellite   Satellite
    ## 4919            Control  Grab N Go         Main    Satellite   Satellite
    ## 4920            Control  Grab N Go         Main    Satellite   Satellite
    ## 4921            Control  Salad Bar         Main    Satellite   Satellite
    ## 4922            Control  Salad Bar Modification    Satellite   Satellite
    ## 4923            Control       Soup         Main    Satellite   Satellite
    ## 4924            Control       Soup         Main    Satellite   Satellite
    ## 4925            Control Quesadilla         Main    Satellite   Satellite
    ## 4926            Control      Grill         Main    Treatment   Treatment
    ## 4927            Control Quesadilla         Main    Satellite   Satellite
    ## 4928            Control  Grab N Go         Side    Satellite   Satellite
    ## 4929            Control      Grill         Side    Treatment   Treatment
    ## 4930            Control      Grill         Main    Treatment   Treatment
    ## 4931            Control Quesadilla         Main    Satellite   Satellite
    ## 4932            Control      Grill         Main    Treatment   Treatment
    ## 4933            Control      Grill         Main    Treatment   Treatment
    ## 4934            Control      Grill         Side    Treatment   Treatment
    ## 4935            Control  Grab N Go         Side    Satellite   Satellite
    ## 4936            Control      Grill Modification    Treatment   Treatment
    ## 4937            Control      Grill         Main    Treatment   Treatment
    ## 4938            Control      Grill Modification    Treatment   Treatment
    ## 4939            Control      Grill Modification    Treatment   Treatment
    ## 4940            Control      Grill Modification    Treatment   Treatment
    ## 4941            Control      Grill Modification    Treatment   Treatment
    ## 4942            Control      Grill Modification    Treatment   Treatment
    ## 4943            Control      Grill Modification    Treatment   Treatment
    ## 4944            Control        Wok         Main    Satellite   Satellite
    ## 4945            Control        Wok         Main    Satellite   Satellite
    ## 4946            Control      Ramen         Main    Treatment   Treatment
    ## 4947            Control        Wok         Main    Satellite   Satellite
    ## 4948            Control      Ramen         Main    Treatment   Treatment
    ## 4949            Control        Wok         Main    Satellite   Satellite
    ## 4950            Control        Wok         Side    Satellite   Satellite
    ## 4951            Control        Wok         Side    Satellite   Satellite
    ## 4952            Control        Wok         Side    Satellite   Satellite
    ## 4953            Control        Wok         Side    Satellite   Satellite
    ## 4954            Control        Wok         Side    Satellite   Satellite
    ## 4955            Control      Pasta         Main    Satellite   Satellite
    ## 4956            Control      Pasta         Main    Satellite   Satellite
    ## 4957            Control      Pizza         Main    Satellite   Satellite
    ## 4958            Control      Pasta Modification    Satellite   Satellite
    ## 4959            Control      Pizza         Main    Satellite   Satellite
    ## 4960            Control      Pasta         Side    Satellite   Satellite
    ## 4961            Control  Breakfast         Main    Satellite   Satellite
    ## 4962            Control  Breakfast         Side    Satellite   Satellite
    ## 4963            Control  Breakfast         Main    Satellite   Satellite
    ## 4964            Control  Breakfast Modification    Satellite   Satellite
    ## 4965            Control  Breakfast         Side    Satellite   Satellite
    ## 4966            Control  Breakfast         Side    Satellite   Satellite
    ## 4967            Control  Breakfast         Side    Satellite   Satellite
    ## 4968            Control  Breakfast         Side    Satellite   Satellite
    ## 4969            Control  Breakfast         Side    Satellite   Satellite
    ## 4970            Control  Breakfast         Side    Satellite   Satellite
    ## 4971            Control       Wrap         Main    Satellite   Satellite
    ## 4972            Control       Wrap         Main    Satellite   Satellite
    ## 4973            Control       Wrap         Side    Satellite   Satellite
    ## 4974            Control       Wrap         Side    Satellite   Satellite
    ## 4975            Control       Wrap         Side    Satellite   Satellite
    ## 4976            Control       Wrap Modification    Satellite   Satellite
    ## 4977            Control  Salad Bar         Main    Satellite   Satellite
    ## 4978            Control  Salad Bar Modification    Satellite   Satellite
    ## 4979            Control  Grab N Go         Main    Satellite   Satellite
    ## 4980            Control  Grab N Go         Main    Satellite   Satellite
    ## 4981            Control  Grab N Go         Main    Satellite   Satellite
    ## 4982            Control       Soup         Main    Satellite   Satellite
    ## 4983            Control       Soup         Main    Satellite   Satellite
    ## 4984            Control Quesadilla         Main    Satellite   Satellite
    ## 4985            Control      Grill         Main    Treatment   Treatment
    ## 4986            Control  Grab N Go         Side    Satellite   Satellite
    ## 4987            Control Quesadilla         Main    Satellite   Satellite
    ## 4988            Control      Grill         Side    Treatment   Treatment
    ## 4989            Control      Grill         Main    Treatment   Treatment
    ## 4990            Control      Grill         Side    Treatment   Treatment
    ## 4991            Control Quesadilla         Main    Satellite   Satellite
    ## 4992            Control      Grill         Main    Treatment   Treatment
    ## 4993            Control      Grill         Main    Treatment   Treatment
    ## 4994            Control      Grill Modification    Treatment   Treatment
    ## 4995            Control  Grab N Go         Side    Satellite   Satellite
    ## 4996            Control      Grill         Main    Treatment   Treatment
    ## 4997            Control      Grill Modification    Treatment   Treatment
    ## 4998            Control      Grill Modification    Treatment   Treatment
    ## 4999            Control      Grill Modification    Treatment   Treatment
    ## 5000            Control      Grill Modification    Treatment   Treatment
    ## 5001            Control        Wok         Main    Satellite   Satellite
    ## 5002            Control        Wok         Main    Satellite   Satellite
    ## 5003            Control      Ramen         Main    Treatment   Treatment
    ## 5004            Control        Wok         Main    Satellite   Satellite
    ## 5005            Control      Ramen         Main    Treatment   Treatment
    ## 5006            Control        Wok         Main    Satellite   Satellite
    ## 5007            Control        Wok         Side    Satellite   Satellite
    ## 5008            Control        Wok         Side    Satellite   Satellite
    ## 5009            Control      Ramen         Main    Treatment   Treatment
    ## 5010            Control        Wok         Side    Satellite   Satellite
    ## 5011            Control        Wok         Side    Satellite   Satellite
    ## 5012            Control  Breakfast         Main    Satellite   Satellite
    ## 5013            Control  Breakfast         Side    Satellite   Satellite
    ## 5014            Control  Breakfast         Main    Satellite   Satellite
    ## 5015            Control  Breakfast Modification    Satellite   Satellite
    ## 5016            Control  Breakfast         Side    Satellite   Satellite
    ## 5017            Control  Breakfast         Side    Satellite   Satellite
    ## 5018            Control  Breakfast         Side    Satellite   Satellite
    ## 5019            Control      Pasta         Main    Satellite   Satellite
    ## 5020            Control      Pasta         Main    Satellite   Satellite
    ## 5021            Control      Pizza         Main    Satellite   Satellite
    ## 5022            Control      Pizza         Main    Satellite   Satellite
    ## 5023            Control      Pasta Modification    Satellite   Satellite
    ## 5024            Control       Wrap         Main    Satellite   Satellite
    ## 5025            Control       Wrap         Side    Satellite   Satellite
    ## 5026            Control       Wrap         Side    Satellite   Satellite
    ## 5027            Control       Wrap         Side    Satellite   Satellite
    ## 5028            Control       Wrap Modification    Satellite   Satellite
    ## 5029            Control  Grab N Go         Main    Satellite   Satellite
    ## 5030            Control  Grab N Go         Main    Satellite   Satellite
    ## 5031            Control  Grab N Go         Main    Satellite   Satellite
    ## 5032            Control  Grab N Go         Main    Satellite   Satellite
    ## 5033            Control  Salad Bar         Main    Satellite   Satellite
    ## 5034            Control  Salad Bar Modification    Satellite   Satellite
    ## 5035            Control       Soup         Main    Satellite   Satellite
    ## 5036            Control       Soup         Main    Satellite   Satellite
    ## 5037            Control       Deli         Main    Satellite   Satellite
    ## 5038            Control Quesadilla         Main    Satellite   Satellite
    ## 5039            Control      Grill         Main    Treatment   Treatment
    ## 5040            Control  Grab N Go         Side    Satellite   Satellite
    ## 5041            Control Quesadilla         Main    Satellite   Satellite
    ## 5042            Control      Grill         Side    Treatment   Treatment
    ## 5043            Control Quesadilla         Main    Satellite   Satellite
    ## 5044            Control      Grill         Side    Treatment   Treatment
    ## 5045            Control      Grill         Main    Treatment   Treatment
    ## 5046            Control      Grill         Main    Treatment   Treatment
    ## 5047            Control      Grill         Main    Treatment   Treatment
    ## 5048            Control  Grab N Go         Side    Satellite   Satellite
    ## 5049            Control      Grill Modification    Treatment   Treatment
    ## 5050            Control      Grill         Main    Treatment   Treatment
    ## 5051            Control      Grill Modification    Treatment   Treatment
    ## 5052            Control      Grill Modification    Treatment   Treatment
    ## 5053            Control      Grill Modification    Treatment   Treatment
    ## 5054            Control      Grill Modification    Treatment   Treatment
    ## 5055            Control        Wok         Main    Satellite   Satellite
    ## 5056            Control        Wok         Main    Satellite   Satellite
    ## 5057            Control      Ramen         Main    Treatment   Treatment
    ## 5058            Control        Wok         Main    Satellite   Satellite
    ## 5059            Control      Ramen         Main    Treatment   Treatment
    ## 5060            Control        Wok         Side    Satellite   Satellite
    ## 5061            Control        Wok         Side    Satellite   Satellite
    ## 5062            Control        Wok         Main    Satellite   Satellite
    ## 5063            Control        Wok         Side    Satellite   Satellite
    ## 5064            Control        Wok         Side    Satellite   Satellite
    ## 5065            Control      Pasta         Main    Satellite   Satellite
    ## 5066            Control      Pasta         Main    Satellite   Satellite
    ## 5067            Control      Pizza         Main    Satellite   Satellite
    ## 5068            Control      Pasta Modification    Satellite   Satellite
    ## 5069            Control      Pizza         Main    Satellite   Satellite
    ## 5070            Control  Breakfast         Main    Satellite   Satellite
    ## 5071            Control  Breakfast         Side    Satellite   Satellite
    ## 5072            Control  Breakfast         Main    Satellite   Satellite
    ## 5073            Control  Breakfast Modification    Satellite   Satellite
    ## 5074            Control  Breakfast         Side    Satellite   Satellite
    ## 5075            Control  Breakfast         Side    Satellite   Satellite
    ## 5076            Control  Breakfast         Side    Satellite   Satellite
    ## 5077            Control  Breakfast         Side    Satellite   Satellite
    ## 5078            Control       Wrap         Main    Satellite   Satellite
    ## 5079            Control       Wrap         Main    Satellite   Satellite
    ## 5080            Control       Wrap         Side    Satellite   Satellite
    ## 5081            Control       Soup         Main    Satellite   Satellite
    ## 5082            Control       Soup         Main    Satellite   Satellite
    ## 5083            Control  Salad Bar         Main    Satellite   Satellite
    ## 5084            Control  Grab N Go         Main    Satellite   Satellite
    ## 5085            Control  Grab N Go         Main    Satellite   Satellite
    ## 5086            Control  Grab N Go         Main    Satellite   Satellite
    ## 5087            Control       Deli         Main    Satellite   Satellite
    ## 5088            Control Quesadilla         Main    Satellite   Satellite
    ## 5089            Control      Grill         Main    Treatment   Treatment
    ## 5090            Control  Grab N Go         Side    Satellite   Satellite
    ## 5091            Control Quesadilla         Main    Satellite   Satellite
    ## 5092            Control      Grill         Side    Treatment   Treatment
    ## 5093            Control      Grill         Side    Treatment   Treatment
    ## 5094            Control      Grill         Main    Treatment   Treatment
    ## 5095            Control      Grill         Main    Treatment   Treatment
    ## 5096            Control Quesadilla         Main    Satellite   Satellite
    ## 5097            Control  Grab N Go         Side    Satellite   Satellite
    ## 5098            Control      Grill Modification    Treatment   Treatment
    ## 5099            Control      Grill         Main    Treatment   Treatment
    ## 5100            Control      Grill         Main    Treatment   Treatment
    ## 5101            Control      Grill Modification    Treatment   Treatment
    ## 5102            Control      Grill Modification    Treatment   Treatment
    ## 5103            Control      Grill Modification    Treatment   Treatment
    ## 5104            Control      Grill Modification    Treatment   Treatment
    ## 5105            Control        Wok         Main    Satellite   Satellite
    ## 5106            Control        Wok         Main    Satellite   Satellite
    ## 5107            Control      Ramen         Main    Treatment   Treatment
    ## 5108            Control        Wok         Main    Satellite   Satellite
    ## 5109            Control      Ramen         Main    Treatment   Treatment
    ## 5110            Control        Wok         Main    Satellite   Satellite
    ## 5111            Control        Wok         Side    Satellite   Satellite
    ## 5112            Control        Wok         Side    Satellite   Satellite
    ## 5113            Control        Wok         Side    Satellite   Satellite
    ## 5114            Control        Wok         Side    Satellite   Satellite
    ## 5115            Control        Wok         Side    Satellite   Satellite
    ## 5116            Control      Pasta         Main    Satellite   Satellite
    ## 5117            Control      Pasta         Main    Satellite   Satellite
    ## 5118            Control      Pizza         Main    Satellite   Satellite
    ## 5119            Control      Pizza         Main    Satellite   Satellite
    ## 5120            Control      Pasta Modification    Satellite   Satellite
    ## 5121            Control  Breakfast         Main    Satellite   Satellite
    ## 5122            Control  Breakfast         Side    Satellite   Satellite
    ## 5123            Control  Breakfast         Main    Satellite   Satellite
    ## 5124            Control  Breakfast Modification    Satellite   Satellite
    ## 5125            Control  Breakfast         Side    Satellite   Satellite
    ## 5126            Control  Breakfast         Side    Satellite   Satellite
    ## 5127            Control  Breakfast         Side    Satellite   Satellite
    ## 5128            Control  Breakfast         Side    Satellite   Satellite
    ## 5129            Control  Breakfast         Side    Satellite   Satellite
    ## 5130            Control       Wrap         Main    Satellite   Satellite
    ## 5131            Control       Wrap         Main    Satellite   Satellite
    ## 5132            Control       Wrap         Side    Satellite   Satellite
    ## 5133            Control       Wrap         Side    Satellite   Satellite
    ## 5134            Control  Grab N Go         Main    Satellite   Satellite
    ## 5135            Control  Grab N Go         Main    Satellite   Satellite
    ## 5136            Control  Grab N Go         Main    Satellite   Satellite
    ## 5137            Control  Grab N Go         Main    Satellite   Satellite
    ## 5138            Control  Salad Bar         Main    Satellite   Satellite
    ## 5139            Control  Salad Bar Modification    Satellite   Satellite
    ## 5140            Control       Soup         Main    Satellite   Satellite
    ## 5141            Control       Soup         Main    Satellite   Satellite
    ## 5142            Control Quesadilla         Main    Satellite   Satellite
    ## 5143            Control      Grill         Main    Treatment   Treatment
    ## 5144            Control Quesadilla         Main    Satellite   Satellite
    ## 5145            Control  Grab N Go         Side    Satellite   Satellite
    ## 5146            Control      Grill         Side    Treatment   Treatment
    ## 5147            Control Quesadilla         Main    Satellite   Satellite
    ## 5148            Control      Grill         Main    Treatment   Treatment
    ## 5149            Control      Grill         Side    Treatment   Treatment
    ## 5150            Control  Grab N Go         Side    Satellite   Satellite
    ## 5151            Control      Grill         Main    Treatment   Treatment
    ## 5152            Control      Grill         Main    Treatment   Treatment
    ## 5153            Control      Grill         Main    Treatment   Treatment
    ## 5154            Control      Grill Modification    Treatment   Treatment
    ## 5155            Control      Grill Modification    Treatment   Treatment
    ## 5156            Control      Grill Modification    Treatment   Treatment
    ## 5157            Control      Grill Modification    Treatment   Treatment
    ## 5158            Control        Wok         Main    Satellite   Satellite
    ## 5159            Control        Wok         Main    Satellite   Satellite
    ## 5160            Control      Ramen         Main    Treatment   Treatment
    ## 5161            Control        Wok         Main    Satellite   Satellite
    ## 5162            Control      Ramen         Main    Treatment   Treatment
    ## 5163            Control        Wok         Main    Satellite   Satellite
    ## 5164            Control        Wok         Side    Satellite   Satellite
    ## 5165            Control        Wok         Side    Satellite   Satellite
    ## 5166            Control        Wok         Side    Satellite   Satellite
    ## 5167            Control        Wok         Side    Satellite   Satellite
    ## 5168            Control  Breakfast         Side    Satellite   Satellite
    ## 5169            Control  Breakfast         Main    Satellite   Satellite
    ## 5170            Control  Breakfast         Main    Satellite   Satellite
    ## 5171            Control  Breakfast Modification    Satellite   Satellite
    ## 5172            Control  Breakfast         Side    Satellite   Satellite
    ## 5173            Control  Breakfast         Side    Satellite   Satellite
    ## 5174            Control  Breakfast         Side    Satellite   Satellite
    ## 5175            Control  Breakfast         Side    Satellite   Satellite
    ## 5176            Control      Pasta         Main    Satellite   Satellite
    ## 5177            Control      Pasta         Main    Satellite   Satellite
    ## 5178            Control      Pizza         Main    Satellite   Satellite
    ## 5179            Control      Pasta Modification    Satellite   Satellite
    ## 5180            Control      Pizza         Main    Satellite   Satellite
    ## 5181            Control       Wrap         Main    Satellite   Satellite
    ## 5182            Control       Wrap         Main    Satellite   Satellite
    ## 5183            Control       Wrap         Side    Satellite   Satellite
    ## 5184            Control       Wrap         Side    Satellite   Satellite
    ## 5185            Control  Salad Bar         Main    Satellite   Satellite
    ## 5186            Control  Salad Bar Modification    Satellite   Satellite
    ## 5187            Control       Soup         Main    Satellite   Satellite
    ## 5188            Control       Soup         Main    Satellite   Satellite
    ## 5189            Control  Grab N Go         Main    Satellite   Satellite
    ## 5190            Control  Grab N Go         Main    Satellite   Satellite
    ## 5191            Control  Grab N Go         Main    Satellite   Satellite

### Distinguishing between meal periods

### Mapping dish-level emissions, water, and cost estimates onto items

``` r
emissions_data
```

    ##     X                                                    recipe sum.kg_co2e.
    ## 1   1                               [CYO Grill Component] Bacon 0.1689180268
    ## 2   2                   [CYO Grill Component] Black Bean Burger 0.1109447123
    ## 3   3                              [CYO Grill Component] Burger 3.8812121746
    ## 4   4                    [CYO Grill Component] Chicken Sandwich 0.3015537456
    ## 5   5                     [CYO Grill Component] Chicken Tenders 0.3764478387
    ## 6   6                               [CYO Grill Component] Fries 0.0408404029
    ## 7   7                   [CYO Grill Component] Impossible Burger 0.2109110881
    ## 8   8                       [CYO Grill Component] Salmon Burger 0.4479260739
    ## 9   9                  [CYO Grill Component] Sweet Potato Fries 0.0422551714
    ## 10 10           [CYO Grill Component] Toppings, American Cheese 0.2603239655
    ## 11 11               [CYO Grill Component] Toppings, Blue Cheese 0.0317565058
    ## 12 12            [CYO Grill Component] Toppings, Cheddar Cheese 0.2603239655
    ## 13 13                   [CYO Grill Component] Toppings, Lettuce 0.0067333350
    ## 14 14        [CYO Grill Component] Toppings, Pepper Jack Cheese 0.2603239655
    ## 15 15          [CYO Grill Component] Toppings, Provolone Cheese 0.2603239655
    ## 16 16                 [CYO Grill Component] Toppings, Red Onion 0.0015372441
    ## 17 17         [CYO Grill Component] Toppings, Sauteed Mushrooms 0.0073640085
    ## 18 18            [CYO Grill Component] Toppings, Sauteed Onions 0.0094660955
    ## 19 19                    [CYO Grill Component] Toppings, Tomato 0.0029741693
    ## 20 20              [CYO Grill Component] Toppings, Vegan Cheese 0.0286154719
    ## 21 21                       [CYO Pasta Component] Alfredo Sauce 0.4017204702
    ## 22 22                       [CYO Pasta Component] Asiago Cheese 0.0651244055
    ## 23 23              [CYO Pasta Component] Breaded Chicken Breast 0.1590743557
    ## 24 24                          [CYO Pasta Component] Breadstick 0.0305102890
    ## 25 25                  [CYO Pasta Component] Creamy Pesto Sauce 0.4518657736
    ## 26 26     [CYO Pasta Component] Creamy Red Pepper Alfredo Sauce 0.4032826588
    ## 27 27        [CYO Pasta Component] Creamy Spinach Alfredo Sauce 0.4019573193
    ## 28 28 [CYO Pasta Component] Creamy Sun Dried Tomato Pesto Sauce 0.4756981148
    ## 29 29                     [CYO Pasta Component] Grilled Chicken 0.1820048526
    ## 30 30            [CYO Pasta Component] Hot Italian Pork Sausage 0.2140008786
    ## 31 31                      [CYO Pasta Component] Linguine Pasta 0.0516246547
    ## 32 32                [CYO Pasta Component] Marinara Pasta Sauce 0.0156404536
    ## 33 33                  [CYO Pasta Component] Meatless Meatballs 0.0607405509
    ## 34 34                [CYO Pasta Component] Mild Italian Sausage 0.2140008786
    ## 35 35               [CYO Pasta Component] Olive & Caper Topping 0.0344150006
    ## 36 36                     [CYO Pasta Component] Parmesan Cheese 0.0503234042
    ## 37 37                     [CYO Pasta Component] Parsley Garnish 0.0006073053
    ## 38 38                         [CYO Pasta Component] Penne Pasta 0.0516246547
    ## 39 39                [CYO Pasta Component] Pork & Beef Meatball 2.1427246765
    ## 40 40             [CYO Pasta Component] Roasted Cherry Tomatoes 0.0282081356
    ## 41 41                    [CYO Pasta Component] Sauteed Broccoli 0.0034914446
    ## 42 42                   [CYO Pasta Component] Sauteed Mushrooms 0.0079807754
    ## 43 43                      [CYO Pasta Component] Sauteed Onions 0.0749064081
    ## 44 44               [CYO Pasta Component] Sauteed Pepper Medley 0.0034314091
    ## 45 45    [CYO Pasta Component] Sauteed Zucchini & Summer Squash 0.0070290224
    ## 46 46                        [CYO Ramen Component] Chicken Bowl 0.3796859151
    ## 47 47                         [CYO Ramen Component] Ginger Tare 0.0010464372
    ## 48 48                           [CYO Ramen Component] Tofu Bowl 0.3065094200

distance between highest-carbon offering and next-lowest offering
between stations

### Separating global dataset into study-level dataasets

With these global changes to the larger sales-count dataset
incorporated, we can now separate the data according to observation
period.

``` r
fall_data <- sales_data %>%
  filter(semester=="Fall 2024")
spring_data <- sales_data %>%
  filter(semester=="Spring 2025")
```

## Study One - Cleaning

The object of the first study is to understand how the nature of diner
meal choices compare across the four menu conditions trialed. More
specifically, we form these comparisons by monitoring changes in the
following four outcome variables:

- `prop_low`: The proportion of lowest-carbon mains purchased at each
  treated station (i.e., the quotient of the number of lowest-carbon
  mains sold during each menu condition divided by the total number of
  mains sold at that station).

- `prop_high`: The proportion of highest-carbon mains purchased at each
  treated station (i.e., the quotient of the number of highest-carbon
  mains sold during each menu condition divided by the total number of
  mains sold at that station).

- `mean_emit`: The average emissions volume associated with the mains
  purchased at each treated station (i.e., the quotient of the emissions
  sum for all mains sold at each treated station divided by the total
  number of mains sold).

- `mean_cost`: The average amount spent on mains at each treated station
  (i.e., the quotient of the summed revenue for all mains sold at each
  treated station divided by the total number of mains sold).

### Between-Condition Observational Differences

We’ll first evaluate the differences in the number of observation days
and sales counts across study periods.

- Differences in the number of observation days across menu conditions:

``` r
fall_data %>%
  group_by(period,date) %>%
  summarise(count=n()) %>%
  mutate(day_count=1) %>%
  summarise(observation_days=sum(day_count))
```

    ## `summarise()` has grouped output by 'period'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 2
    ##   period             observation_days
    ##   <chr>                         <dbl>
    ## 1 Carbon Label                     10
    ## 2 Control                           8
    ## 3 Default                          10
    ## 4 Multimodal                        7
    ## 5 Multimodal (Extra)               10

- Differences in global sales volume across menu conditions:

``` r
fall_data %>%
  group_by(period,date) %>%
  summarise(count=n()) %>%
  group_by(period) %>%
  summarise(sales_count=sum(count))
```

    ## `summarise()` has grouped output by 'period'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 2
    ##   period             sales_count
    ##   <chr>                    <int>
    ## 1 Carbon Label               505
    ## 2 Control                    409
    ## 3 Default                    499
    ## 4 Multimodal                 353
    ## 5 Multimodal (Extra)         448

### Grill - Mains - Prop

``` r
fall_data %>%
  filter(period=="Control") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(19+125+776+68+60)) %>%
  mutate(condition="Control")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition
    ##   <chr>                                 <int>  <dbl> <chr>    
    ## 1 Black Bean Burger                        19 0.0181 Control  
    ## 2 Grilled Chicken Breast Sandwich         125 0.119  Control  
    ## 3 Grilled Hamburger                       776 0.740  Control  
    ## 4 Seared Salmon Burger                     68 0.0649 Control  
    ## 5 Trillium Grill Impossible Burger         60 0.0573 Control

``` r
fall_data %>%
  filter(period=="Carbon Label") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(32+159+935+74+85)) %>%
  mutate(condition="Carbon Label")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition   
    ##   <chr>                                 <int>  <dbl> <chr>       
    ## 1 Black Bean Burger                        32 0.0249 Carbon Label
    ## 2 Grilled Chicken Breast Sandwich         159 0.124  Carbon Label
    ## 3 Grilled Hamburger                       935 0.728  Carbon Label
    ## 4 Seared Salmon Burger                     74 0.0576 Carbon Label
    ## 5 Trillium Grill Impossible Burger         85 0.0661 Carbon Label

``` r
fall_data %>%
  filter(period=="Default") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(33+167+904+76+90)) %>%
  mutate(condition="Default")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition
    ##   <chr>                                 <int>  <dbl> <chr>    
    ## 1 Black Bean Burger                        33 0.0260 Default  
    ## 2 Grilled Chicken Breast Sandwich         167 0.131  Default  
    ## 3 Grilled Hamburger                       904 0.712  Default  
    ## 4 Seared Salmon Burger                     76 0.0598 Default  
    ## 5 Trillium Grill Impossible Burger         90 0.0709 Default

``` r
fall_data %>%
  filter(period=="Multimodal") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(27+84+624+59+50)) %>%
  mutate(condition="Multimodal")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition 
    ##   <chr>                                 <int>  <dbl> <chr>     
    ## 1 Black Bean Burger                        27 0.0320 Multimodal
    ## 2 Grilled Chicken Breast Sandwich          84 0.0995 Multimodal
    ## 3 Grilled Hamburger                       624 0.739  Multimodal
    ## 4 Seared Salmon Burger                     59 0.0699 Multimodal
    ## 5 Trillium Grill Impossible Burger         50 0.0592 Multimodal

``` r
fall_data %>%
  filter(period=="Multimodal (Extra)") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(14+81+533+66+57)) %>%
  mutate(condition="Multimodal (Extra)")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition         
    ##   <chr>                                 <int>  <dbl> <chr>             
    ## 1 Black Bean Burger                        14 0.0186 Multimodal (Extra)
    ## 2 Grilled Chicken Breast Sandwich          81 0.108  Multimodal (Extra)
    ## 3 Grilled Hamburger                       533 0.710  Multimodal (Extra)
    ## 4 Seared Salmon Burger                     66 0.0879 Multimodal (Extra)
    ## 5 Trillium Grill Impossible Burger         57 0.0759 Multimodal (Extra)

``` r
fall_data %>%
  filter(period=="Multimodal (Extra)"| period=="Multimodal") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(41+165+1157+125+107)) %>%
  mutate(condition="Multimodal (Full)")
```

    ## # A tibble: 5 × 4
    ##   item                             item_count   prop condition        
    ##   <chr>                                 <int>  <dbl> <chr>            
    ## 1 Black Bean Burger                        41 0.0257 Multimodal (Full)
    ## 2 Grilled Chicken Breast Sandwich         165 0.103  Multimodal (Full)
    ## 3 Grilled Hamburger                      1157 0.725  Multimodal (Full)
    ## 4 Seared Salmon Burger                    125 0.0784 Multimodal (Full)
    ## 5 Trillium Grill Impossible Burger        107 0.0671 Multimodal (Full)

### Ramen - Mains - Prop

``` r
fall_data %>%
  filter(period=="Control") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(563+108)) %>%
  mutate(condition="Control")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition
    ##   <chr>                   <int> <dbl> <chr>    
    ## 1 Bowl Ramen Chicken        563 0.839 Control  
    ## 2 Bowl Ramen Tofu           108 0.161 Control

``` r
fall_data %>%
  filter(period=="Carbon Label") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(685+135)) %>%
  mutate(condition="Carbon Label")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition   
    ##   <chr>                   <int> <dbl> <chr>       
    ## 1 Bowl Ramen Chicken        685 0.835 Carbon Label
    ## 2 Bowl Ramen Tofu           135 0.165 Carbon Label

``` r
fall_data %>%
  filter(period=="Default") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(698+172)) %>%
  mutate(condition="Default")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition
    ##   <chr>                   <int> <dbl> <chr>    
    ## 1 Bowl Ramen Chicken        698 0.802 Default  
    ## 2 Bowl Ramen Tofu           172 0.198 Default

``` r
fall_data %>%
  filter(period=="Multimodal") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(483+109)) %>%
  mutate(condition="Multimodal")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition 
    ##   <chr>                   <int> <dbl> <chr>     
    ## 1 Bowl Ramen Chicken        483 0.816 Multimodal
    ## 2 Bowl Ramen Tofu           109 0.184 Multimodal

``` r
fall_data %>%
  filter(period=="Multimodal (Extra)") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(377+73)) %>%
  mutate(condition="Multimodal (Extra)")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition         
    ##   <chr>                   <int> <dbl> <chr>             
    ## 1 Bowl Ramen Chicken        377 0.838 Multimodal (Extra)
    ## 2 Bowl Ramen Tofu            73 0.162 Multimodal (Extra)

``` r
fall_data %>%
  filter(period=="Multimodal (Extra)" | period=="Multimodal") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(860+182)) %>%
  mutate(condition="Multimodal (Full)")
```

    ## # A tibble: 2 × 4
    ##   item               item_count  prop condition        
    ##   <chr>                   <int> <dbl> <chr>            
    ## 1 Bowl Ramen Chicken        860 0.825 Multimodal (Full)
    ## 2 Bowl Ramen Tofu           182 0.175 Multimodal (Full)

### Grill - Mains and Modifications - Prop

``` r
fall_data %>%
  filter(period=="Control") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main" | item_cat=="Modification") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) 
```

    ## # A tibble: 12 × 2
    ##    item                             item_count
    ##    <chr>                                 <int>
    ##  1 + Beef Patty                             97
    ##  2 ADD Burger Salmon Grilled                 1
    ##  3 ADD Cheese                               45
    ##  4 ADD Chicken Breast                       21
    ##  5 Add Egg .99                              18
    ##  6 Add Impossible Burger Patty               1
    ##  7 Add Sausage 2 Patty                      20
    ##  8 Black Bean Burger                        19
    ##  9 Grilled Chicken Breast Sandwich         125
    ## 10 Grilled Hamburger                       776
    ## 11 Seared Salmon Burger                     68
    ## 12 Trillium Grill Impossible Burger         60

differnce in carbon estiamte for highest and lowest number of options to
choose between

Sides and modifications not factored in currently

also measure spillover across other stations during same meal interval

measure spillover across other meal interval

### Foot Traffic

``` r
foot_traffic_data %>%
  select(date,time,count,period) %>%
  distinct(time)
```

    ##        time
    ## 1  10:00 AM
    ## 2  10:15 AM
    ## 3  10:30 AM
    ## 4  10:45 AM
    ## 5  11:00 AM
    ## 6  11:15 AM
    ## 7  11:30 AM
    ## 8  11:45 AM
    ## 9  12:00 PM
    ## 10 12:15 PM
    ## 11 12:30 PM
    ## 12 12:45 PM
    ## 13  1:00 PM
    ## 14  1:15 PM
    ## 15  1:30 PM
    ## 16  1:45 PM
    ## 17  2:00 PM
    ## 18  2:15 PM
    ## 19  2:30 PM
    ## 20  2:45 PM
    ## 21  3:00 PM
    ## 22  8:00 AM
    ## 23  8:15 AM
    ## 24  8:30 AM
    ## 25  8:45 AM
    ## 26  9:00 AM
    ## 27  9:15 AM
    ## 28  9:30 AM
    ## 29  9:45 AM
    ## 30  7:30 AM
    ## 31  7:45 AM
    ## 32  3:15 PM
    ## 33  7:15 AM
    ## 34  4:00 PM
    ## 35  6:15 AM
    ## 36  3:30 PM

``` r
foot_traffic_data %>%
  select(date,time,count,period) %>%
  group_by(period,time) %>%
  summarise(transaction_volume=mean(count)) 
```

    ## `summarise()` has grouped output by 'period'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 98 × 3
    ## # Groups:   period [3]
    ##    period  time     transaction_volume
    ##    <chr>   <chr>                 <dbl>
    ##  1 Control 10:00 AM              44.5 
    ##  2 Control 10:15 AM              27.7 
    ##  3 Control 10:30 AM               8.82
    ##  4 Control 10:45 AM              14.0 
    ##  5 Control 11:00 AM              97.6 
    ##  6 Control 11:15 AM              91.5 
    ##  7 Control 11:30 AM             126.  
    ##  8 Control 11:45 AM              88.4 
    ##  9 Control 12:00 PM             106.  
    ## 10 Control 12:15 PM             140.  
    ## # ℹ 88 more rows

``` r
head(5)
```

    ## [1] 5

``` r
foot_traffic_data %>%
  select(date,time,count,period) %>%
  group_by(period,time) %>%
  summarise(transaction_volume=mean(count)) %>%
  ggplot(aes(x=time,y=transaction_volume,fill=period)) + 
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("6:15 AM","6:30 AM","6:45 AM","7:00 AM","7:15 AM","7:30 AM","7:45 AM","8:00 AM","8:15 AM","8:30 AM","8:45 AM","9:00 AM","9:15 AM","9:30 AM","9:45 AM","10:00 AM","10:15 AM","10:30 AM","10:45 AM","11:00 AM","11:15 AM","11:30 AM","11:45 AM","12:00 PM","12:15 PM","12:30 PM","12:45 PM","1:00 PM","1:15 PM","1:30 PM","1:45 PM","2:00 PM","2:15 PM","2:30 PM","2:45 PM","3:00 PM","3:15 PM","3:30 PM","3:45 PM","4:00 PM"))
```

    ## `summarise()` has grouped output by 'period'. You can override using the
    ## `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

sales_data %\>% mutate(item_cat=case_when(item==“Quesadilla Deluxe
Trillium”~“Main”, item==“Grilled Hamburger”~“Main”, item==“Fried Chicken
Tenders”~“Main”, item==“Burrito Una Mano Trillium BYO”~“Main”,
item==“French Fries”~“Side”, item==“Quesadilla Cheese”~“Ma
