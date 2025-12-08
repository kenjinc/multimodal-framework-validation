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
ingredient_level_footprint_data <- read.csv("/Users/kenjinchang/github/multimodal-framework-validation/data/parent-data/ingredient-level-footprint-calculations.csv")
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
relevant stations and, subsequently, designate them (using
`station_type`) as either satellite (i.e., used to measure effects on
sales outside of the treated stations) or treatment-receiving (i.e.,used
to measure within-station changes in food choice). More specifically,
this will offer us an initial, within-period indicator of reactance
(i.e., whether there are decreases in sales that temporally correlate
with the implementation of any of the various treatment-menu
conditions).

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
```

We will additionally create a new variable, `meal_period`, to
distinguish between the different service windows represented within
`sales_data`. More specifically, to offer a contrary between-indicator
of reactance, we will use the coded categories (i.e., “Breakfast” and
“Lunch”) to monitor for whether the phases during which the different
menu conditions were implemented were associated with any decreases in
sales volumes.

``` r
sales_data <- sales_data %>%
  mutate(meal_period=case_when(station=="Breakfast"~"Breakfast",
                                station=="Deli"~"Lunch",
                                station=="Grab N Go"~"Lunch",
                                station=="Grill"~"Lunch",
                                station=="Pasta"~"Lunch",
                                station=="Pizza"~"Lunch",
                                station=="Quesadilla"~"Lunch",
                                station=="Ramen"~"Lunch",
                                station=="Salad Bar"~"Lunch",
                                station=="Soup"~"Lunch",
                                station=="Wok"~"Lunch",
                                station=="Wrap"~"Lunch")) 
```

### Mapping dish-level emissions, water, and cost estimates onto items

In our companion repo, we had—during the planning phases of this
project—collated life-cycle analysis data to estimate the carbon and
blue-water costs associated with each of the dishes on offer at the two
treated stations. In part, the purpose of this recipe modeling was to
enable precise estimations for the carbon- and water-footprint labels
used during some of the treatment phases of the study. Another virtue of
this work, however, is that it also enables us to understand how diner
food choices map on to their corresponding climate and resource costs.
In other words, we are able to use these figures to help calculate the
mean emissions and water costs associated with item sales at each
station across the various menu conditions trialed. This enables us to
reasonably detect whether the sustainability of diners’ food choices
improved during the treatment phases, outside of the simpler proportions
used to investigate whether the highest- and lowest-emitting options
were chosen less and more, respectively.

To avoid premature rounding, we will recalculate the emissions and
resource costs associated with the various mains, modifications, and
sides on offer at both the “Grill” and “Ramen” stations, rather than use
the estimates used to populate the descriptive footprint labels.

We’ll start with process by pairing each of the qualifying offerings
with their corresponding water and emissions costs. As a reminder, these
are the sales item we need to find correspond carbon- and water-cost
data for:

``` r
sales_data %>%
  filter(station=="Grill"|station=="Ramen"|station=="Pasta") %>%
  distinct(item) %>%
  arrange()
```

    ##                                item
    ## 1                 Grilled Hamburger
    ## 2                      French Fries
    ## 3   Grilled Chicken Breast Sandwich
    ## 4              Seared Salmon Burger
    ## 5  Trillium Grill Impossible Burger
    ## 6                Sweet Potato Fries
    ## 7                      + Beef Patty
    ## 8                 Black Bean Burger
    ## 9       Add Impossible Burger Patty
    ## 10                      Add Egg .99
    ## 11                       ADD Cheese
    ## 12              Add Sausage 2 Patty
    ## 13        ADD Burger Salmon Grilled
    ## 14               Bowl Ramen Chicken
    ## 15                  Bowl Ramen Tofu
    ## 16      Create Your Pasta Bowl MEAT
    ## 17       Create Your Pasta Bowl VEG
    ## 18                   Add Extra Meat
    ## 19         Side Bread Pasta Station
    ## 20               ADD Chicken Breast
    ## 21               Add Extra Toppings

We’ll begin by pairing the calculated water- and carbon-footprint
estimates to each corresponding composite item (i.e., an item comprised
of more than one ingredient, as defined by the research partner’s recipe
system):

``` r
composite_item_pairing <- ingredient_level_footprint_data %>%
  group_by(recipe) %>%
  summarise(ind_carbon_cost=sum(kg_co2e),ind_water_cost=sum(l_h2o_blue)) %>%
  filter(recipe=="[CYO Grill Component] Burger"|recipe=="[CYO Grill Component] Fries"|recipe=="[CYO Grill Component] Chicken Sandwich"|recipe=="[CYO Grill Component] Salmon Burger"|recipe=="[CYO Grill Component] Impossible Burger"|recipe=="[CYO Grill Component] Sweet Potato Fries"|recipe=="[CYO Grill Component] Black Bean Burger"|recipe=="[CYO Ramen Component] Chicken Bowl"|recipe=="[CYO Ramen Component] Tofu Bowl"|recipe=="[CYO Ramen Component] Ginger Tare") %>%
  mutate(item=case_when(recipe=="[CYO Grill Component] Black Bean Burger"~"Black Bean Burger",
                        recipe=="[CYO Grill Component] Burger"~"Grilled Hamburger",
                        recipe=="[CYO Grill Component] Chicken Sandwich"~"Grilled Chicken Breast Sandwich",
                        recipe=="[CYO Grill Component] Fries"~"French Fries",
                        recipe=="[CYO Grill Component] Impossible Burger"~"Trillium Grill Impossible Burger",
                        recipe=="[CYO Grill Component] Salmon Burger"~"Seared Salmon Burger",
                        recipe=="[CYO Grill Component] Sweet Potato Fries"~"Sweet Potato Fries",
                        recipe=="[CYO Ramen Component] Chicken Bowl"~"Bowl Ramen Chicken",
                        recipe=="[CYO Ramen Component] Ginger Tare"~"Add Extra Toppings",
                        recipe=="[CYO Ramen Component] Tofu Bowl"~"Bowl Ramen Tofu")) %>%
  select(item,ind_carbon_cost,ind_water_cost)
```

This leaves us with seven items left to pair, each of which are listed
within the recipe system as single ingredients (i.e., simple recipes).
We’ll handle the “ADD Cheese” modification separately because, unlike
the remaining six items, this option requires us to calculate the
average cost associated with each of the six different topping varieties
offered.

``` r
variable_item_pairing <- ingredient_level_footprint_data %>%
  filter(station=="Grill") %>%
  filter(recipe=="[CYO Grill Component] Toppings, American Cheese"|recipe=="[CYO Grill Component] Toppings, Cheddar Cheese"|recipe=="[CYO Grill Component] Toppings, Pepper Jack Cheese"|recipe=="[CYO Grill Component] Toppings, Provolone Cheese"|recipe=="[CYO Grill Component] Toppings, Blue Cheese"|recipe=="[CYO Grill Component] Toppings, Vegan Cheese") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="ADD Cheese") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue))
variable_item_pairing
```

    ## # A tibble: 1 × 3
    ##   item       ind_carbon_cost ind_water_cost
    ##   <chr>                <dbl>          <dbl>
    ## 1 ADD Cheese           0.184           3.20

PASTA - average for sauce, average for pasta, average for add meat ,
then add together

``` r
pasta_item_pairing_sauces <- ingredient_level_footprint_data %>%
  filter(station=="Pasta") %>%
  filter(recipe=="[CYO Pasta Component] Alfredo Sauce"|recipe=="[CYO Pasta Component] Creamy Spinach Alfredo Sauce"|recipe=="[CYO Pasta Component] Marinara Pasta Sauce"|recipe=="[CYO Pasta Component] Creamy Pesto Sauce"|recipe=="[CYO Pasta Component] Creamy Sun Dried Tomato Pesto Sauce"|recipe=="[CYO Pasta Component] Creamy Red Pepper Alfredo Sauce") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="Sauces") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue))
```

``` r
pasta_item_pairing_noodles <- ingredient_level_footprint_data %>%
  filter(station=="Pasta") %>%
  filter(recipe=="[CYO Pasta Component] Penne Pasta"|recipe=="[CYO Pasta Component] Linguine Pasta")  %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="Noodles") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue))
```

``` r
pasta_item_pairing_veggies <- ingredient_level_footprint_data %>%
  filter(station=="Pasta") %>%
  filter(recipe=="[CYO Pasta Component] Sauteed Broccoli"|recipe=="[CYO Pasta Component] Roasted Cherry Tomatoes"|recipe=="[CYO Pasta Component] Sauteed Mushrooms"|recipe=="[CYO Pasta Component] Olive & Caper Topping"|recipe=="[CYO Pasta Component] Sauteed Onions"|recipe=="[CYO Pasta Component] Sauteed Pepper Medley"|recipe=="[CYO Pasta Component] Sauteed Zucchini & Summer Squash"|recipe=="[CYO Pasta Component] Meatless Meatballs") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="Vegetable and Mushroom Options") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue))
```

``` r
pasta_item_pairing_meats <- ingredient_level_footprint_data %>%
  filter(station=="Pasta") %>%
  filter(recipe=="[CYO Pasta Component] Mild Italian Sausage"|recipe=="[CYO Pasta Component] Hot Italian Pork Sausage"|recipe=="[CYO Pasta Component] Pork & Beef Meatball"|recipe=="[CYO Pasta Component] Grilled Chicken"|recipe=="[CYO Pasta Component] Breaded Chicken Breast") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="Meat Options") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue)) 
```

``` r
pasta_item_pairing_cheeses <- ingredient_level_footprint_data %>%
  filter(station=="Pasta") %>%
  filter(recipe=="[CYO Pasta Component] Parmesan Cheese"|recipe=="[CYO Pasta Component] Asiago Cheese") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="Cheese Options") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue)) 
```

``` r
pasta_item_pairing <- bind_rows(pasta_item_pairing_sauces,pasta_item_pairing_noodles,pasta_item_pairing_veggies,pasta_item_pairing_meats,pasta_item_pairing_cheeses)
```

``` r
vegetarian_pasta_pairing <- pasta_item_pairing %>%
  filter(item=="Sauces"|item=="Noodles"|item=="Vegetable and Mushroom Options"|item=="Cheese Options") %>%
  mutate(item="Create Your Pasta Bowl VEG") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=sum(ind_carbon_cost),ind_water_cost=sum(ind_water_cost))
vegetarian_pasta_pairing
```

    ## # A tibble: 1 × 3
    ##   item                       ind_carbon_cost ind_water_cost
    ##   <chr>                                <dbl>          <dbl>
    ## 1 Create Your Pasta Bowl VEG           0.140           10.6

``` r
meat_pasta_pairing <- pasta_item_pairing %>%
  mutate(item="Create Your Pasta Bowl MEAT") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=sum(ind_carbon_cost),ind_water_cost=sum(ind_water_cost))
meat_pasta_pairing
```

    ## # A tibble: 1 × 3
    ##   item                        ind_carbon_cost ind_water_cost
    ##   <chr>                                 <dbl>          <dbl>
    ## 1 Create Your Pasta Bowl MEAT           0.626           47.8

With that sorted, we can now move on to the last six remaining simple
recipes:

``` r
simple_item_pairing <- ingredient_level_footprint_data %>%
  filter(ingredient=="80/20 Beef Patty" | ingredient=="\"Impossible Burger\" Patty [Patty \"Impossible Burger\" Trillium]" | ingredient=="Bacon Pork Raw" | ingredient=="5 Oz Bnls Skinles Chicken Breast" | ingredient=="Salmon Burger Patty" | ingredient=="Egg Whole Fresh Medium Flat [Egg Sous Vide]") %>%
  select(ingredient,kg_co2e,l_h2o_blue) %>%
  mutate(item=case_when(ingredient=="80/20 Beef Patty"~"+ Beef Patty",
                        ingredient=="5 Oz Bnls Skinles Chicken Breast"~"ADD Chicken Breast",
                        ingredient=="Salmon Burger Patty"~"ADD Burger Salmon Grilled",
                        ingredient=="\"Impossible Burger\" Patty [Patty \"Impossible Burger\" Trillium]"~"Add Impossible Burger Patty",
                        ingredient=="Bacon Pork Raw"~"Add Sausage 2 Patty",
                        ingredient=="Egg Whole Fresh Medium Flat [Egg Sous Vide]"~"Add Egg .99")) %>%
  rename(ind_carbon_cost=kg_co2e,ind_water_cost=l_h2o_blue) %>%
  select(item,ind_carbon_cost,ind_water_cost) %>%
  distinct()
```

Now that we’ve mapped the corresponding emissions- and resource-costs
associated with each of the items offered at the treatment station (and
control station), we can perform a join to incorporate this information
into `sales_data`. Before that, though, we need to add the monetary
costs of each item, too, to help us understand whether sales revenue
varied across the different menu conditions trialled.

While we have most of the pricing information available to accomplish
this, we assume a flat 2.99 fee for all additional patty modifications,
with the exception of the “Add Sausage 2 Patty” modification, which
instead maps on to the bacon addition presented on the “Grill” menu. We
also assume a \$0.99 fee “Add Extra Toppings” modification at the
“Ramen” station, which maps on to the Ginger Tare that appears in our
recipe modeling.

``` r
item_pairing <- bind_rows(composite_item_pairing,variable_item_pairing,simple_item_pairing,vegetarian_pasta_pairing,meat_pasta_pairing)
item_pairing <- item_pairing %>%
  mutate(ind_dollar_cost=case_when(item=="Black Bean Burger"~8.69,
                                  item=="Grilled Hamburger"~8.99,
                                  item=="Grilled Chicken Breast Sandwich"~8.49,
                                  item=="French Fries"~2.99,
                                  item=="Trillium Grill Impossible Burger"~10.49,
                                  item=="Seared Salmon Burger"~8.49,
                                  item=="Sweet Potato Fries"~2.99,
                                  item=="Bowl Ramen Chicken"~9.49,
                                  item=="Add Extra Toppings"~.99,
                                  item=="Bowl Ramen Tofu"~9.49,
                                  item=="ADD Cheese"~1.99,
                                  item=="+ Beef Patty"~2.99,
                                  item=="ADD Chicken Breast"~2.99,
                                  item=="ADD Burger Salmon Grilled"~2.99,
                                  item=="Add Impossible Burger Patty"~2.99,
                                  item=="Add Sausage 2 Patty"~1.99,
                                  item=="Add Egg .99"~.99,
                                  item=="Create Your Pasta Bowl VEG"~8.49,
                                  item=="Create Your Pasta Bowl MEAT"~8.99))
item_pairing
```

    ## # A tibble: 19 × 4
    ##    item                           ind_carbon_cost ind_water_cost ind_dollar_cost
    ##    <chr>                                    <dbl>          <dbl>           <dbl>
    ##  1 Black Bean Burger                      0.111          55.0               8.69
    ##  2 Grilled Hamburger                      3.88           88.9               8.99
    ##  3 Grilled Chicken Breast Sandwi…         0.302          69.5               8.49
    ##  4 French Fries                           0.0408          2.73              2.99
    ##  5 Trillium Grill Impossible Bur…         0.211          23.3              10.5 
    ##  6 Seared Salmon Burger                   0.448          44.7               8.49
    ##  7 Sweet Potato Fries                     0.0423          3.98              2.99
    ##  8 Bowl Ramen Chicken                     0.380         105.                9.49
    ##  9 Add Extra Toppings                     0.00105         0.0221            0.99
    ## 10 Bowl Ramen Tofu                        0.307          92.0               9.49
    ## 11 ADD Cheese                             0.184           3.20              1.99
    ## 12 + Beef Patty                           3.81           79.9               2.99
    ## 13 ADD Chicken Breast                     0.227          60.4               2.99
    ## 14 ADD Burger Salmon Grilled              0.374          35.7               2.99
    ## 15 Add Impossible Burger Patty            0.152          13.0               2.99
    ## 16 Add Sausage 2 Patty                    0.169          34.6               1.99
    ## 17 Add Egg .99                            0.0904         25.3               0.99
    ## 18 Create Your Pasta Bowl VEG             0.140          10.6               8.49
    ## 19 Create Your Pasta Bowl MEAT            0.626          47.8               8.99

Now, we can add the individual, item-level costs for each of the mains,
modifications, and sides sold at the two treatment stations.

``` r
sales_data <- left_join(sales_data,item_pairing,join_by(item))
```

In addition to adding `ind_carbon_cost`, `ind_water_cost`, and
`ind_dollar_cost` to `sales_data`, which map on to the individual costs
of each of the items sold at the treatment stations, we will also add
`corr_carbon_cost`, `corr_water_cost`, and `corr_dollar_cost`, which
will map on to the corespoding costs of each of the items sold at the
treatment stations. These variables will instead represent the daily
aggregated costs associated with the sales of those items, based on the
total quantity sold.

``` r
sales_data <- sales_data %>%
  mutate(corr_carbon_cost=count*ind_carbon_cost) %>%
  mutate(corr_water_cost=count*ind_water_cost) %>%
  mutate(corr_dollar_cost=count*ind_dollar_cost) 
write.csv(sales_data,"/Users/kenjinchang/github/multimodal-framework-validation/data/cleaned_daily-sales-counts.csv")
```

### Separating global dataset into study-level dataasets

With these global changes to `sales_data` now incorporated, we can
separate the data according to observation period.

``` r
fall_data <- sales_data %>%
  filter(semester=="Fall 2024")
spring_data <- sales_data %>%
  filter(semester=="Spring 2025")
```

## Cleaning and Analysis (Fall 2024)

The object of the first study is to understand how diner meal choices
compared across the four menu conditions trialed. More specifically, we
wish to form these comparisons by monitoring specific changes in the
following outcome variables:

- `prop_low`: The proportion of lowest-carbon mains purchased at each
  treated station (i.e., the quotient of the number of lowest-carbon
  mains sold during each menu condition divided by the total number of
  mains sold at that station).

- `prop_high`: The proportion of highest-carbon mains purchased at each
  treated station (i.e., the quotient of the number of highest-carbon
  mains sold during each menu condition divided by the total number of
  mains sold at that station).

- `mean_carbon_cost`: The mean emissions cost across the mains, sides,
  and modifications sold at each treated station (i.e., the quotient of
  the aggregated emissions costs for the items sold at each treated
  station divided by the total number of items sold).

- `mean_spend`: The average revenue gained on mains, sides, and
  modifications at each treated station (i.e., the quotient of the
  aggregated revenue for gained for the items sold at each treated
  station divided by the total number of items sold).

### Proportion of lowest-carbon selections

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Bowl Ramen Tofu" & menu_condition=="Carbon Label" ~ total_count/(685+135),
                            item=="Black Bean Burger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Bowl Ramen Tofu" & menu_condition=="Control" ~ total_count/(563+108),
                            item=="Black Bean Burger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Bowl Ramen Tofu" & menu_condition=="Default" ~ total_count/(698+172),
                            item=="Black Bean Burger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107),
                            item=="Bowl Ramen Tofu" & menu_condition=="Multimodal" ~ total_count/(860+182))) %>%
  drop_na(prop_low)
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 8 × 5
    ## # Groups:   menu_condition, station [8]
    ##   menu_condition station item              total_count prop_low
    ##   <chr>          <chr>   <chr>                   <int>    <dbl>
    ## 1 Carbon Label   Grill   Black Bean Burger          32   0.0249
    ## 2 Carbon Label   Ramen   Bowl Ramen Tofu           135   0.165 
    ## 3 Control        Grill   Black Bean Burger          19   0.0181
    ## 4 Control        Ramen   Bowl Ramen Tofu           108   0.161 
    ## 5 Default        Grill   Black Bean Burger          33   0.0260
    ## 6 Default        Ramen   Bowl Ramen Tofu           172   0.198 
    ## 7 Multimodal     Grill   Black Bean Burger          41   0.0257
    ## 8 Multimodal     Ramen   Bowl Ramen Tofu           182   0.175

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Bowl Ramen Tofu" & menu_condition=="Carbon Label" ~ total_count/(685+135),
                            item=="Black Bean Burger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Bowl Ramen Tofu" & menu_condition=="Control" ~ total_count/(563+108),
                            item=="Black Bean Burger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Bowl Ramen Tofu" & menu_condition=="Default" ~ total_count/(698+172),
                            item=="Black Bean Burger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107),
                            item=="Bowl Ramen Tofu" & menu_condition=="Multimodal" ~ total_count/(860+182))) %>%
  drop_na(prop_low) %>%
  ggplot(aes(x=menu_condition,y=prop_low,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->
Grill only

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Black Bean Burger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Black Bean Burger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Black Bean Burger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107))) %>%
  drop_na(prop_low) %>%
  ggplot(aes(x=menu_condition,y=prop_low,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Grilled Hamburger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Grilled Hamburger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Grilled Hamburger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Grilled Hamburger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107))) %>%
  drop_na(prop_high) %>%
  ggplot(aes(x=menu_condition,y=prop_high,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_impossible=case_when(item=="Trillium Grill Impossible Burger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Trillium Grill Impossible Burger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Trillium Grill Impossible Burger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Trillium Grill Impossible Burger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107))) %>%
  drop_na(prop_impossible) %>%
  ggplot(aes(x=menu_condition,y=prop_impossible,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_chicken=case_when(item=="Grilled Chicken Breast Sandwich" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Grilled Chicken Breast Sandwich" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Grilled Chicken Breast Sandwich" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Grilled Chicken Breast Sandwich" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107))) %>%
  drop_na(prop_chicken) %>%
  ggplot(aes(x=menu_condition,y=prop_chicken,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_salmon=case_when(item=="Seared Salmon Burger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Seared Salmon Burger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Seared Salmon Burger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Seared Salmon Burger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107))) %>%
  drop_na(prop_salmon) %>%
  ggplot(aes(x=menu_condition,y=prop_salmon,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

daily prop-low

``` r
daily_prop_low_fall_data <- fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(47),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(41),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(49),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="11-Nov"~total_count/(81),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(55),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="12-Nov"~total_count/(95),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(57),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="13-Nov"~total_count/(91),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="14-Nov"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="15-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(47),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="16-Oct"~total_count/(104),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(41),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="17-Oct"~total_count/(82),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(46),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="18-Nov"~total_count/(77),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="18-Oct"~total_count/(65),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(14),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="19-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(113),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(23),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="20-Nov"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="21-Nov"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="21-Oct"~total_count/(85),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="22-Nov"~total_count/(57),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="22-Oct"~total_count/(83),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="23-Oct"~total_count/(92),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="24-Oct"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="25-Oct"~total_count/(53),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(51),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(80),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(82),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(85),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(92),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(84),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(73),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(77))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_vline <- as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20"))
fall_vline <- which(daily_prop_low_fall_data$date %in% fall_vline)
```

``` r
ggplot(daily_prop_low_fall_data,aes(x=date,y=prop_low,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        menu_condition=="Control"~total_count/(19+125+776+68+60),
                        menu_condition=="Default"~total_count/(33+167+904+76+90),
                        menu_condition=="Multimodal"~total_count/(41+165+1157+125+107))) %>%
  filter(item=="Black Bean Burger")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item              total_count   prop
    ##   <chr>          <chr>   <chr>                   <int>  <dbl>
    ## 1 Carbon Label   Grill   Black Bean Burger          32 0.0249
    ## 2 Control        Grill   Black Bean Burger          19 0.0181
    ## 3 Default        Grill   Black Bean Burger          33 0.0260
    ## 4 Multimodal     Grill   Black Bean Burger          41 0.0257

``` r
fall_prop_low_grill_kruskal_data <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  arrange(menu_condition)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
kruskal.test(prop_low~menu_condition,data=fall_prop_low_grill_kruskal_data) 
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  prop_low by menu_condition
    ## Kruskal-Wallis chi-squared = 1.7887, df = 3, p-value = 0.6174

``` r
fall_prop_low_grill_aov_data <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  arrange(menu_condition)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
shapiro.test(fall_prop_low_grill_aov_data$prop_low) ## Shapiro-Wilk test, test assumption of normality, p-value is greater than 0.05, do not reject the null hypothesis, data follow a normal distribution
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fall_prop_low_grill_aov_data$prop_low
    ## W = 0.95434, p-value = 0.08591

``` r
oneway.test(prop_low~menu_condition,data=fall_prop_low_grill_aov_data,var.equal=TRUE)
```

    ## 
    ##  One-way analysis of means
    ## 
    ## data:  prop_low and menu_condition
    ## F = 0.66076, num df = 3, denom df = 39, p-value = 0.5812

``` r
fall_prop_low_grill_ttest_data <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  filter(menu_condition=="Control"|menu_condition=="Default") %>%
  arrange(menu_condition)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
t.test(prop_low~menu_condition,data=fall_prop_low_grill_ttest_data)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  prop_low by menu_condition
    ## t = -1.1465, df = 15.216, p-value = 0.2693
    ## alternative hypothesis: true difference in means between group Control and group Default is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.019704507  0.005909555
    ## sample estimates:
    ## mean in group Control mean in group Default 
    ##            0.01938321            0.02628068

``` r
alt_fall_prop_low_grill_aov_data <- fall_data %>% 
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  filter(item=="Black Bean Burger") %>%
  select(menu_condition,count) 
shapiro.test(alt_fall_prop_low_grill_aov_data$count) #violates normality assumption, proceed with Kruskal
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  alt_fall_prop_low_grill_aov_data$count
    ## W = 0.90415, p-value = 0.001676

``` r
kruskal.test(count~menu_condition,data=alt_fall_prop_low_grill_aov_data) 
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  count by menu_condition
    ## Kruskal-Wallis chi-squared = 2.1724, df = 3, p-value = 0.5374

``` r
fall_prop_low_grill <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop_low,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05)) +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.01512977,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.02190272,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.02298425,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.02270533,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.018312977) +
  annotate("point",x=as.Date("2024-11-4"),y=0.02490272) +
  annotate("point",x=as.Date("2024-11-18"),y=0.02598425) +
  annotate("point",x=as.Date("2024-12-7"),y=0.02570533) +
  labs(title="Grill Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_prop_low_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Black Bean Burger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Black Bean Burger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Black Bean Burger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Black Bean Burger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=menu_condition,y=prop_low)) +
  geom_violin() + 
  geom_jitter() +
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal")) +
  coord_flip()
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(685+135),
                        menu_condition=="Control"~total_count/(563+108),
                        menu_condition=="Default"~total_count/(698+172),
                        menu_condition=="Multimodal"~total_count/(860+182))) %>%
  filter(item=="Bowl Ramen Tofu")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item            total_count  prop
    ##   <chr>          <chr>   <chr>                 <int> <dbl>
    ## 1 Carbon Label   Ramen   Bowl Ramen Tofu         135 0.165
    ## 2 Control        Ramen   Bowl Ramen Tofu         108 0.161
    ## 3 Default        Ramen   Bowl Ramen Tofu         172 0.198
    ## 4 Multimodal     Ramen   Bowl Ramen Tofu         182 0.175

``` r
fall_prop_low_ramen_kruskal_data <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(34+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(37+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(42+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="11-Nov"~total_count/(69+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(46+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="12-Nov"~total_count/(76+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(50+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="13-Nov"~total_count/(74+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="14-Nov"~total_count/(78+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="15-Nov"~total_count/(59+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(40+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="16-Oct"~total_count/(88+16),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(35+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="17-Oct"~total_count/(68+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(39+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="18-Nov"~total_count/(66+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="18-Oct"~total_count/(44+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(10+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="19-Nov"~total_count/(70+28),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(94+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(21+2),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="20-Nov"~total_count/(80+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="21-Nov"~total_count/(77+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="21-Oct"~total_count/(71+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="22-Nov"~total_count/(49+8),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="22-Oct"~total_count/(73+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="23-Oct"~total_count/(83+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="24-Oct"~total_count/(92+15),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(56+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="25-Oct"~total_count/(44+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(45+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(65+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(67+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(76+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(70+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(79+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(84+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(65+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(70+22),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(73+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(58+18),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(90+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(83+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(59+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(57+20))) %>%
drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
kruskal.test(prop_low~menu_condition,data=fall_prop_low_ramen_kruskal_data) 
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  prop_low by menu_condition
    ## Kruskal-Wallis chi-squared = 2.3428, df = 3, p-value = 0.5044

``` r
fall_prop_low_ramen_ttest_data <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(34+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(37+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(42+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="11-Nov"~total_count/(69+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(46+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="12-Nov"~total_count/(76+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(50+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="13-Nov"~total_count/(74+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="14-Nov"~total_count/(78+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="15-Nov"~total_count/(59+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(40+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="16-Oct"~total_count/(88+16),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(35+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="17-Oct"~total_count/(68+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(39+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="18-Nov"~total_count/(66+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="18-Oct"~total_count/(44+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(10+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="19-Nov"~total_count/(70+28),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(94+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(21+2),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="20-Nov"~total_count/(80+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="21-Nov"~total_count/(77+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="21-Oct"~total_count/(71+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="22-Nov"~total_count/(49+8),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="22-Oct"~total_count/(73+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="23-Oct"~total_count/(83+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="24-Oct"~total_count/(92+15),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(56+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="25-Oct"~total_count/(44+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(45+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(65+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(67+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(76+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(70+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(79+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(84+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(65+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(70+22),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(73+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(58+18),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(90+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(83+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(59+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(57+20))) %>%
drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  filter(menu_condition=="Control"|menu_condition=="Default") %>%
  arrange(menu_condition)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
t.test(prop_low~menu_condition,data=fall_prop_low_ramen_ttest_data)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  prop_low by menu_condition
    ## t = -0.94439, df = 11.498, p-value = 0.3644
    ## alternative hypothesis: true difference in means between group Control and group Default is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08692669  0.03453465
    ## sample estimates:
    ## mean in group Control mean in group Default 
    ##             0.1675834             0.1937794

``` r
fall_prop_low_ramen <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(34+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(37+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(42+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="11-Nov"~total_count/(69+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(46+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="12-Nov"~total_count/(76+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(50+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="13-Nov"~total_count/(74+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="14-Nov"~total_count/(78+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="15-Nov"~total_count/(59+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(40+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="16-Oct"~total_count/(88+16),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(35+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="17-Oct"~total_count/(68+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(39+7),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="18-Nov"~total_count/(66+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="18-Oct"~total_count/(44+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(10+4),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="19-Nov"~total_count/(70+28),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(94+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(21+2),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="20-Nov"~total_count/(80+19),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="21-Nov"~total_count/(77+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="21-Oct"~total_count/(71+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Default"&date=="22-Nov"~total_count/(49+8),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="22-Oct"~total_count/(73+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="23-Oct"~total_count/(83+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="24-Oct"~total_count/(92+15),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(56+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Control"&date=="25-Oct"~total_count/(44+9),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(45+6),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(65+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(67+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(76+21),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(70+12),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(79+10),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(84+13),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(65+20),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(70+22),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(73+11),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(58+18),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(90+17),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(83+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(59+14),
                             item=="Bowl Ramen Tofu"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(57+20))) %>%
drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop_low,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.1509538,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.1546341,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.1877011,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.1646641,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.1609538) +
  annotate("point",x=as.Date("2024-11-4"),y=0.1646341) +
  annotate("point",x=as.Date("2024-11-18"),y=0.1977011) +
  annotate("point",x=as.Date("2024-12-7"),y=0.1746641) +
  labs(title="Ramen Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_prop_low_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(32+159+935+74+85+685+135),
                        menu_condition=="Control"~total_count/(19+125+776+68+60+563+108),
                        menu_condition=="Default"~total_count/(33+167+904+76+90+698+172),
                        menu_condition=="Multimodal"~total_count/(41+165+1157+125+107+860+182))) %>%
   mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(menu_condition,item) %>%
  summarise(prop=sum(prop)) %>%
  filter(item=="Lowest-Carbon Offering")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   menu_condition [4]
    ##   menu_condition item                     prop
    ##   <chr>          <chr>                   <dbl>
    ## 1 Carbon Label   Lowest-Carbon Offering 0.0793
    ## 2 Control        Lowest-Carbon Offering 0.0739
    ## 3 Default        Lowest-Carbon Offering 0.0958
    ## 4 Multimodal     Lowest-Carbon Offering 0.0846

``` r
fall_prop_low_treatment_kruskal_data <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"High",
                        item=="Grilled Hamburger"~"High",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop_low=case_when(item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(100+16+25),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(89+4+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(89+9+19),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="11-Nov"~total_count/(150+15+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(115+10+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="12-Nov"~total_count/(165+22+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(99+8+16),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="13-Nov"~total_count/(181+19+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="14-Nov"~total_count/(177+24+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="15-Nov"~total_count/(131+21+18),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(95+8+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="16-Oct"~total_count/(179+18+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(96+9+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="17-Oct"~total_count/(177+15+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(86+8+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="18-Nov"~total_count/(148+18+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="18-Oct"~total_count/(110+23+21),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(43+4+17),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="19-Nov"~total_count/(192+31+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(191+25+15),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(45+3+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="20-Nov"~total_count/(172+22+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="21-Nov"~total_count/(186+24+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="21-Oct"~total_count/(176+15+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="22-Nov"~total_count/(100+9+24),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="22-Oct"~total_count/(200+12+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="23-Oct"~total_count/(183+11+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="24-Oct"~total_count/(199+20+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(123+12+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="25-Oct"~total_count/(115+13+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(97+9+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(161+15+27),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(169+19+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(173+25+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(165+17+32),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(186+11+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(201+14+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(145+24+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(188+28+49),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(180+13+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(134+23+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(194+19+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(181+18+40),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(139+15+26),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(153+24+29))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  arrange(menu_condition)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
kruskal.test(prop_low~menu_condition,data=fall_prop_low_treatment_kruskal_data) 
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  prop_low by menu_condition
    ## Kruskal-Wallis chi-squared = 5.5399, df = 3, p-value = 0.1363

``` r
fall_prop_low_treatment_ttest_data <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"High",
                        item=="Grilled Hamburger"~"High",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop_low=case_when(item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(100+16+25),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(89+4+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(89+9+19),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="11-Nov"~total_count/(150+15+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(115+10+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="12-Nov"~total_count/(165+22+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(99+8+16),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="13-Nov"~total_count/(181+19+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="14-Nov"~total_count/(177+24+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="15-Nov"~total_count/(131+21+18),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(95+8+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="16-Oct"~total_count/(179+18+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(96+9+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="17-Oct"~total_count/(177+15+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(86+8+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="18-Nov"~total_count/(148+18+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="18-Oct"~total_count/(110+23+21),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(43+4+17),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="19-Nov"~total_count/(192+31+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(191+25+15),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(45+3+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="20-Nov"~total_count/(172+22+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="21-Nov"~total_count/(186+24+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="21-Oct"~total_count/(176+15+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="22-Nov"~total_count/(100+9+24),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="22-Oct"~total_count/(200+12+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="23-Oct"~total_count/(183+11+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="24-Oct"~total_count/(199+20+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(123+12+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="25-Oct"~total_count/(115+13+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(97+9+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(161+15+27),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(169+19+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(173+25+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(165+17+32),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(186+11+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(201+14+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(145+24+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(188+28+49),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(180+13+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(134+23+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(194+19+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(181+18+40),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(139+15+26),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(153+24+29))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  arrange(menu_condition) %>%
  filter(menu_condition=="Control"|menu_condition=="Default")
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
t.test(prop_low~menu_condition,data=fall_prop_low_treatment_ttest_data)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  prop_low by menu_condition
    ## t = -1.344, df = 10.078, p-value = 0.2084
    ## alternative hypothesis: true difference in means between group Control and group Default is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.04490178  0.01109125
    ## sample estimates:
    ## mean in group Control mean in group Default 
    ##            0.07769193            0.09459720

``` r
fall_prop_low_treatment <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"High",
                        item=="Grilled Hamburger"~"High",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop_low=case_when(item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(100+16+25),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(89+4+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(89+9+19),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="11-Nov"~total_count/(150+15+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(115+10+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="12-Nov"~total_count/(165+22+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(99+8+16),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="13-Nov"~total_count/(181+19+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="14-Nov"~total_count/(177+24+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="15-Nov"~total_count/(131+21+18),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(95+8+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="16-Oct"~total_count/(179+18+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(96+9+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="17-Oct"~total_count/(177+15+43),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(86+8+23),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="18-Nov"~total_count/(148+18+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="18-Oct"~total_count/(110+23+21),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(43+4+17),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="19-Nov"~total_count/(192+31+42),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(191+25+15),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(45+3+14),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="20-Nov"~total_count/(172+22+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="21-Nov"~total_count/(186+24+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="21-Oct"~total_count/(176+15+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Default"&date=="22-Nov"~total_count/(100+9+24),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="22-Oct"~total_count/(200+12+34),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="23-Oct"~total_count/(183+11+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="24-Oct"~total_count/(199+20+33),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(123+12+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Control"&date=="25-Oct"~total_count/(115+13+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(97+9+20),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(161+15+27),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(169+19+37),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(173+25+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(165+17+32),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(186+11+41),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(201+14+28),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(145+24+30),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(188+28+49),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(180+13+29),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(134+23+22),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(194+19+31),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(181+18+40),
                             item=="Lowest-Carbon Offering"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(139+15+26),
                             item=="Lowest-Carbon Offering"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(153+24+29))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=prop_low,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.06788016,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.07333492,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.08979439,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.07856579,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.07388016) +
  annotate("point",x=as.Date("2024-11-4"),y=0.07933492) +
  annotate("point",x=as.Date("2024-11-18"),y=0.09579439) +
  annotate("point",x=as.Date("2024-12-7"),y=0.08456579) +
  labs(title="Ramen & Grill Stations (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_prop_low_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"&item=="Create Your Pasta Bowl MEAT"~total_count/(1159+249),
                        menu_condition=="Carbon Label"&item=="Create Your Pasta Bowl VEG"~total_count/(1159+249),
                        menu_condition=="Control"&item=="Create Your Pasta Bowl MEAT"~total_count/(951+207),
                        menu_condition=="Control"&item=="Create Your Pasta Bowl VEG"~total_count/(951+207),
                        menu_condition=="Default"&item=="Create Your Pasta Bowl MEAT"~total_count/(1101+240),
                        menu_condition=="Default"&item=="Create Your Pasta Bowl VEG"~total_count/(1101+240),
                        menu_condition=="Multimodal"&item=="Create Your Pasta Bowl MEAT"~total_count/(1068+261),
                        menu_condition=="Multimodal"&item=="Create Your Pasta Bowl VEG"~total_count/(1068+261)))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 8 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item                        total_count  prop
    ##   <chr>          <chr>   <chr>                             <int> <dbl>
    ## 1 Carbon Label   Pasta   Create Your Pasta Bowl MEAT        1159 0.823
    ## 2 Carbon Label   Pasta   Create Your Pasta Bowl VEG          249 0.177
    ## 3 Control        Pasta   Create Your Pasta Bowl MEAT         951 0.821
    ## 4 Control        Pasta   Create Your Pasta Bowl VEG          207 0.179
    ## 5 Default        Pasta   Create Your Pasta Bowl MEAT        1101 0.821
    ## 6 Default        Pasta   Create Your Pasta Bowl VEG          240 0.179
    ## 7 Multimodal     Pasta   Create Your Pasta Bowl MEAT        1068 0.804
    ## 8 Multimodal     Pasta   Create Your Pasta Bowl VEG          261 0.196

``` r
fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(1159+249),
                        menu_condition=="Control"~total_count/(951+207),
                        menu_condition=="Default"~total_count/(1101+240),
                        menu_condition=="Multimodal"~total_count/(1068+261))) %>%
  filter(item=="Create Your Pasta Bowl VEG")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item                       total_count  prop
    ##   <chr>          <chr>   <chr>                            <int> <dbl>
    ## 1 Carbon Label   Pasta   Create Your Pasta Bowl VEG         249 0.177
    ## 2 Control        Pasta   Create Your Pasta Bowl VEG         207 0.179
    ## 3 Default        Pasta   Create Your Pasta Bowl VEG         240 0.179
    ## 4 Multimodal     Pasta   Create Your Pasta Bowl VEG         261 0.196

``` r
fall_prop_low_control_kruskal_data <- fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(87+8),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(45+9),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(41+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="11-Nov"~total_count/(119+25),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(34+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="12-Nov"~total_count/(112+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(45+5),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="13-Nov"~total_count/(113+26),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="14-Nov"~total_count/(103+31),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="15-Nov"~total_count/(79+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(46+7),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="16-Oct"~total_count/(149+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(30+10),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="17-Oct"~total_count/(128+33),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(25+11),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="18-Nov"~total_count/(116+30),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="18-Oct"~total_count/(83+13),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(12+4),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="19-Nov"~total_count/(115+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(128+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(16+5),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="20-Nov"~total_count/(143+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="21-Nov"~total_count/(131+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="21-Oct"~total_count/(124+37),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="22-Nov"~total_count/(70+9),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="22-Oct"~total_count/(132+26),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="23-Oct"~total_count/(124+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="24-Oct"~total_count/(131+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(55+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="25-Oct"~total_count/(80+14),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(29+6),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127+38),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(108+25),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(116+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(136+23),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(120+27),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(142+27),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(118+34),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(113+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(103+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(83+12),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(136+30),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(132+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(92+16),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(108+32))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Create Your Pasta Bowl VEG"~"Lowest-Carbon Offering")) %>%
  ungroup() %>%
  select(menu_condition,prop_low) %>%
  arrange(menu_condition) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
kruskal.test(prop_low~menu_condition,data=fall_prop_low_control_kruskal_data) 
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  prop_low by menu_condition
    ## Kruskal-Wallis chi-squared = 3.1068, df = 3, p-value = 0.3755

``` r
fall_prop_low_control <- fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_low=case_when(item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(87+8),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(45+9),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(41+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="11-Nov"~total_count/(119+25),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(34+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="12-Nov"~total_count/(112+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(45+5),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="13-Nov"~total_count/(113+26),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="14-Nov"~total_count/(103+31),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="15-Nov"~total_count/(79+15),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(46+7),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="16-Oct"~total_count/(149+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(30+10),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="17-Oct"~total_count/(128+33),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(25+11),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="18-Nov"~total_count/(116+30),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="18-Oct"~total_count/(83+13),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(12+4),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="19-Nov"~total_count/(115+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(128+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(16+5),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="20-Nov"~total_count/(143+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="21-Nov"~total_count/(131+24),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="21-Oct"~total_count/(124+37),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Default"&date=="22-Nov"~total_count/(70+9),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="22-Oct"~total_count/(132+26),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="23-Oct"~total_count/(124+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="24-Oct"~total_count/(131+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(55+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Control"&date=="25-Oct"~total_count/(80+14),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(29+6),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127+38),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(108+25),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(116+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(136+23),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(120+27),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(142+27),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(118+34),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(113+32),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(103+20),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(83+12),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(136+30),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(132+28),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(92+16),
                             item=="Create Your Pasta Bowl VEG"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(108+32))) %>%
  drop_na(prop_low) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Create Your Pasta Bowl VEG"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop_low,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.1687565,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.1668466,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.1689709,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.1863883,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.1787565) +
  annotate("point",x=as.Date("2024-11-4"),y=0.1768466) +
  annotate("point",x=as.Date("2024-11-18"),y=0.1789709) +
  annotate("point",x=as.Date("2024-12-7"),y=0.1963883) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_prop_low_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
fall_prop_low <- ggarrange(fall_prop_low_ramen,fall_prop_low_grill,fall_prop_low_treatment,fall_prop_low_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
fall_prop_low <- annotate_figure(fall_prop_low,top=text_grob("Daily Sales Percentages of Lowest-Carbon Offerings (%)", 
               color="black",face="bold",size = 12))
ggsave(filename="fall_prop_low.png",plot=fall_prop_low,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
fall_prop_low
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

### Proportion of highest-emitting selections

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Grilled Hamburger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Bowl Ramen Chicken" & menu_condition=="Carbon Label" ~ total_count/(685+135),
                            item=="Grilled Hamburger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Bowl Ramen Chicken" & menu_condition=="Control" ~ total_count/(563+108),
                            item=="Grilled Hamburger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Bowl Ramen Chicken" & menu_condition=="Default" ~ total_count/(698+172),
                            item=="Grilled Hamburger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107),
                            item=="Bowl Ramen Chicken" & menu_condition=="Multimodal" ~ total_count/(860+182))) %>%
  drop_na(prop_high)
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 8 × 5
    ## # Groups:   menu_condition, station [8]
    ##   menu_condition station item               total_count prop_high
    ##   <chr>          <chr>   <chr>                    <int>     <dbl>
    ## 1 Carbon Label   Grill   Grilled Hamburger          935     0.728
    ## 2 Carbon Label   Ramen   Bowl Ramen Chicken         685     0.835
    ## 3 Control        Grill   Grilled Hamburger          776     0.740
    ## 4 Control        Ramen   Bowl Ramen Chicken         563     0.839
    ## 5 Default        Grill   Grilled Hamburger          904     0.712
    ## 6 Default        Ramen   Bowl Ramen Chicken         698     0.802
    ## 7 Multimodal     Grill   Grilled Hamburger         1157     0.725
    ## 8 Multimodal     Ramen   Bowl Ramen Chicken         860     0.825

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Grilled Hamburger" & menu_condition=="Carbon Label" ~ total_count/(32+159+935+74+85),
                            item=="Bowl Ramen Chicken" & menu_condition=="Carbon Label" ~ total_count/(685+135),
                            item=="Grilled Hamburger" & menu_condition=="Control" ~ total_count/(19+125+776+68+60),
                            item=="Bowl Ramen Chicken" & menu_condition=="Control" ~ total_count/(563+108),
                            item=="Grilled Hamburger" & menu_condition=="Default" ~ total_count/(33+167+904+76+90),
                            item=="Bowl Ramen Chicken" & menu_condition=="Default" ~ total_count/(698+172),
                            item=="Grilled Hamburger" & menu_condition=="Multimodal" ~ total_count/(41+165+1157+125+107),
                            item=="Bowl Ramen Chicken" & menu_condition=="Multimodal" ~ total_count/(860+182))) %>%
  drop_na(prop_high) %>%
  ggplot(aes(x=menu_condition,y=prop_high,fill=station)) +
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

Now on a daily level to see if there are diminishing effects over time
across each menu condition

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,station) %>%
  summarise(daily_station_count=sum(count)) 
```

    ## `summarise()` has grouped output by 'date'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 90 × 3
    ## # Groups:   date [45]
    ##    date   station daily_station_count
    ##    <chr>  <chr>                 <int>
    ##  1 1-Nov  Grill                    94
    ##  2 1-Nov  Ramen                    47
    ##  3 10-Dec Grill                    72
    ##  4 10-Dec Ramen                    41
    ##  5 11-Dec Grill                    68
    ##  6 11-Dec Ramen                    49
    ##  7 11-Nov Grill                   112
    ##  8 11-Nov Ramen                    81
    ##  9 12-Dec Grill                    99
    ## 10 12-Dec Ramen                    55
    ## # ℹ 80 more rows

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(47),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(41),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(49),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="11-Nov"~total_count/(81),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(55),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="12-Nov"~total_count/(95),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(57),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="13-Nov"~total_count/(91),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="14-Nov"~total_count/(99),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="15-Nov"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(47),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="16-Oct"~total_count/(104),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(41),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="17-Oct"~total_count/(82),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(46),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="18-Nov"~total_count/(77),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="18-Oct"~total_count/(65),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(14),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="19-Nov"~total_count/(98),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(113),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(23),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="20-Nov"~total_count/(99),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="21-Nov"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="21-Oct"~total_count/(85),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="22-Nov"~total_count/(57),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="22-Oct"~total_count/(83),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="23-Oct"~total_count/(92),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="24-Oct"~total_count/(107),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(66),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="25-Oct"~total_count/(53),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(51),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(80),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(82),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(89),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(85),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(92),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(84),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(107),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(73),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(77))) %>%
  drop_na(prop_high) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

    ## # A tibble: 90 × 6
    ## # Groups:   date, menu_condition, station [90]
    ##    date   menu_condition station item               total_count prop_high
    ##    <chr>  <chr>          <chr>   <chr>                    <int>     <dbl>
    ##  1 1-Nov  Carbon Label   Grill   Grilled Hamburger           66     0.702
    ##  2 1-Nov  Carbon Label   Ramen   Bowl Ramen Chicken          34     0.723
    ##  3 10-Dec Multimodal     Grill   Grilled Hamburger           52     0.722
    ##  4 10-Dec Multimodal     Ramen   Bowl Ramen Chicken          37     0.902
    ##  5 11-Dec Multimodal     Grill   Grilled Hamburger           47     0.691
    ##  6 11-Dec Multimodal     Ramen   Bowl Ramen Chicken          42     0.857
    ##  7 11-Nov Default        Grill   Grilled Hamburger           81     0.723
    ##  8 11-Nov Default        Ramen   Bowl Ramen Chicken          69     0.852
    ##  9 12-Dec Multimodal     Grill   Grilled Hamburger           69     0.697
    ## 10 12-Dec Multimodal     Ramen   Bowl Ramen Chicken          46     0.836
    ## # ℹ 80 more rows

``` r
daily_prop_high_fall_data <- fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(47),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(41),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(49),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="11-Nov"~total_count/(81),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(55),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="12-Nov"~total_count/(95),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(57),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="13-Nov"~total_count/(91),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="14-Nov"~total_count/(99),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="15-Nov"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(47),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="16-Oct"~total_count/(104),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(41),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="17-Oct"~total_count/(82),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(46),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="18-Nov"~total_count/(77),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="18-Oct"~total_count/(65),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(14),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="19-Nov"~total_count/(98),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(113),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(23),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="20-Nov"~total_count/(99),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="21-Nov"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="21-Oct"~total_count/(85),
                             item=="Grilled Hamburger"&menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                             item=="Bowl Ramen Chicken"&menu_condition=="Default"&date=="22-Nov"~total_count/(57),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="22-Oct"~total_count/(83),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="23-Oct"~total_count/(92),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="24-Oct"~total_count/(107),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(66),
                             item=="Grilled Hamburger"&menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                             item=="Bowl Ramen Chicken"&menu_condition=="Control"&date=="25-Oct"~total_count/(53),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(51),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(80),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(82),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(89),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(85),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(92),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(84),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(76),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(107),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(97),
                             item=="Grilled Hamburger"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                             item=="Bowl Ramen Chicken"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(73),
                             item=="Grilled Hamburger"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128),
                             item=="Bowl Ramen Chicken"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(77))) %>%
  drop_na(prop_high) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
ggplot(daily_prop_high_fall_data,aes(x=date,y=prop_high,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        menu_condition=="Control"~total_count/(19+125+776+68+60),
                        menu_condition=="Default"~total_count/(33+167+904+76+90),
                        menu_condition=="Multimodal"~total_count/(41+165+1157+125+107))) %>%
  filter(item=="Grilled Hamburger")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item              total_count  prop
    ##   <chr>          <chr>   <chr>                   <int> <dbl>
    ## 1 Carbon Label   Grill   Grilled Hamburger         935 0.728
    ## 2 Control        Grill   Grilled Hamburger         776 0.740
    ## 3 Default        Grill   Grilled Hamburger         904 0.712
    ## 4 Multimodal     Grill   Grilled Hamburger        1157 0.725

``` r
fall_prop_high_grill <- daily_prop_high_fall_data %>%
  filter(item=="Grilled Hamburger") %>%
  mutate(item=case_when(item=="Grilled Hamburger"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop_high,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.7304580,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.7176265,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.7018110,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.7153918,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.7404580) +
  annotate("point",x=as.Date("2024-11-4"),y=0.7276265) +
  annotate("point",x=as.Date("2024-11-18"),y=0.7118110) +
  annotate("point",x=as.Date("2024-12-7"),y=0.7253918) +
  labs(title="Grill Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
fall_prop_high_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(685+135),
                        menu_condition=="Control"~total_count/(563+108),
                        menu_condition=="Default"~total_count/(698+172),
                        menu_condition=="Multimodal"~total_count/(860+182))) %>%
  filter(item=="Bowl Ramen Chicken")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item               total_count  prop
    ##   <chr>          <chr>   <chr>                    <int> <dbl>
    ## 1 Carbon Label   Ramen   Bowl Ramen Chicken         685 0.835
    ## 2 Control        Ramen   Bowl Ramen Chicken         563 0.839
    ## 3 Default        Ramen   Bowl Ramen Chicken         698 0.802
    ## 4 Multimodal     Ramen   Bowl Ramen Chicken         860 0.825

``` r
fall_prop_high_ramen <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="1-Nov"~total_count/(34+13),
                            date=="10-Dec"~total_count/(37+4),
                             date=="11-Dec"~total_count/(42+7),
                             date=="11-Nov"~total_count/(69+12),
                             date=="12-Dec"~total_count/(46+9),
                             date=="12-Nov"~total_count/(76+19),
                             date=="13-Dec"~total_count/(50+7),
                             date=="13-Nov"~total_count/(74+17),
                             date=="14-Nov"~total_count/(78+21),
                             date=="15-Nov"~total_count/(59+17),
                             date=="16-Dec"~total_count/(40+7),
                             date=="16-Oct"~total_count/(88+16),
                             date=="17-Dec"~total_count/(35+6),
                             date=="17-Oct"~total_count/(68+14),
                             date=="18-Dec"~total_count/(39+7),
                             date=="18-Nov"~total_count/(66+11),
                             date=="18-Oct"~total_count/(44+21),
                             date=="19-Dec"~total_count/(10+4),
                             date=="19-Nov"~total_count/(70+28),
                             date=="2-Dec"~total_count/(94+19),
                             date=="20-Dec"~total_count/(21+2),
                             date=="20-Nov"~total_count/(80+19),
                             date=="21-Nov"~total_count/(77+20),
                             date=="21-Oct"~total_count/(71+14),
                             date=="22-Nov"~total_count/(49+8),
                             date=="22-Oct"~total_count/(73+10),
                             date=="23-Oct"~total_count/(83+9),
                             date=="24-Oct"~total_count/(92+15),
                             date=="25-Nov"~total_count/(56+10),
                             date=="25-Oct"~total_count/(44+9),
                             date=="26-Nov"~total_count/(45+6),
                             date=="28-Oct"~total_count/(65+11),
                             date=="29-Oct"~total_count/(67+13),
                             date=="3-Dec"~total_count/(76+21),
                             date=="30-Oct"~total_count/(70+12),
                             date=="31-Oct"~total_count/(79+10),
                             date=="4-Dec"~total_count/(84+13),
                             date=="4-Nov"~total_count/(65+20),
                             date=="5-Dec"~total_count/(70+22),
                             date=="5-Nov"~total_count/(73+11),
                             date=="6-Dec"~total_count/(58+18),
                             date=="6-Nov"~total_count/(90+17),
                             date=="7-Nov"~total_count/(83+14),
                             date=="8-Nov"~total_count/(59+14),
                             date=="9-Dec"~total_count/(57+20))) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  filter(item=="Bowl Ramen Chicken") %>% # Highest-carbon offering
  mutate(item=case_when(item=="Bowl Ramen Chicken"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.8290462,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.8253659,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.7922989,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.8153359,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.8390462) +
  annotate("point",x=as.Date("2024-11-4"),y=0.8353659) +
  annotate("point",x=as.Date("2024-11-18"),y=0.8022989) +
  annotate("point",x=as.Date("2024-12-7"),y=0.8253359) +
  labs(title="Ramen Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_prop_high_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(32+159+935+74+85+685+135),
                        menu_condition=="Control"~total_count/(19+125+776+68+60+563+108),
                        menu_condition=="Default"~total_count/(33+167+904+76+90+698+172),
                        menu_condition=="Multimodal"~total_count/(41+165+1157+125+107+860+182))) %>%
   mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(menu_condition,item) %>%
  summarise(prop=sum(prop)) %>%
  filter(item=="Highest-Carbon Offering")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   menu_condition [4]
    ##   menu_condition item                     prop
    ##   <chr>          <chr>                   <dbl>
    ## 1 Carbon Label   Highest-Carbon Offering 0.770
    ## 2 Control        Highest-Carbon Offering 0.779
    ## 3 Default        Highest-Carbon Offering 0.749
    ## 4 Multimodal     Highest-Carbon Offering 0.765

``` r
fall_prop_high_treatment <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop_high=case_when(item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(100+16+25),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(89+4+20),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(89+9+19),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="11-Nov"~total_count/(150+15+28),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(115+10+29),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="12-Nov"~total_count/(165+22+42),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(99+8+16),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="13-Nov"~total_count/(181+19+30),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="14-Nov"~total_count/(177+24+43),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="15-Nov"~total_count/(131+21+18),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(95+8+14),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="16-Oct"~total_count/(179+18+30),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(96+9+23),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="17-Oct"~total_count/(177+15+43),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(86+8+23),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="18-Nov"~total_count/(148+18+41),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="18-Oct"~total_count/(110+23+21),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(43+4+17),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="19-Nov"~total_count/(192+31+42),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(191+25+15),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(45+3+14),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="20-Nov"~total_count/(172+22+34),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="21-Nov"~total_count/(186+24+31),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="21-Oct"~total_count/(176+15+37),
                             item=="Highest-Carbon Offering"&menu_condition=="Default"&date=="22-Nov"~total_count/(100+9+24),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="22-Oct"~total_count/(200+12+34),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="23-Oct"~total_count/(183+11+33),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="24-Oct"~total_count/(199+20+33),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(123+12+29),
                             item=="Highest-Carbon Offering"&menu_condition=="Control"&date=="25-Oct"~total_count/(115+13+22),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(97+9+20),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(161+15+27),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(169+19+37),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(173+25+30),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(165+17+32),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(186+11+41),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(201+14+28),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(145+24+30),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(188+28+49),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(180+13+29),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(134+23+22),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(194+19+31),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(181+18+40),
                             item=="Highest-Carbon Offering"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(139+15+26),
                             item=="Highest-Carbon Offering"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(153+24+29))) %>%
  drop_na(prop_high) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=prop_high,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.7689412,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.7595962,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.7385981,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.7548843,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.7789412) +
  annotate("point",x=as.Date("2024-11-4"),y=0.7695962) +
  annotate("point",x=as.Date("2024-11-18"),y=0.7485981) +
  annotate("point",x=as.Date("2024-12-7"),y=0.7648843) +
  labs(title="Ramen & Grill Stations (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10))
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_prop_high_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"~total_count/(1159+249),
                        menu_condition=="Control"~total_count/(951+207),
                        menu_condition=="Default"~total_count/(1101+240),
                        menu_condition=="Multimodal"~total_count/(1068+261))) %>%
  filter(item=="Create Your Pasta Bowl MEAT")
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition, station [4]
    ##   menu_condition station item                        total_count  prop
    ##   <chr>          <chr>   <chr>                             <int> <dbl>
    ## 1 Carbon Label   Pasta   Create Your Pasta Bowl MEAT        1159 0.823
    ## 2 Control        Pasta   Create Your Pasta Bowl MEAT         951 0.821
    ## 3 Default        Pasta   Create Your Pasta Bowl MEAT        1101 0.821
    ## 4 Multimodal     Pasta   Create Your Pasta Bowl MEAT        1068 0.804

``` r
fall_prop_high_control <- fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop_high=case_when(item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(87+8),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="10-Dec"~total_count/(45+9),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="11-Dec"~total_count/(41+15),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="11-Nov"~total_count/(119+25),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="12-Dec"~total_count/(34+15),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="12-Nov"~total_count/(112+20),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="13-Dec"~total_count/(45+5),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="13-Nov"~total_count/(113+26),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="14-Nov"~total_count/(103+31),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="15-Nov"~total_count/(79+15),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="16-Dec"~total_count/(46+7),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="16-Oct"~total_count/(149+24),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="17-Dec"~total_count/(30+10),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="17-Oct"~total_count/(128+33),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="18-Dec"~total_count/(25+11),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="18-Nov"~total_count/(116+30),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="18-Oct"~total_count/(83+13),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="19-Dec"~total_count/(12+4),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="19-Nov"~total_count/(115+28),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="2-Dec"~total_count/(128+24),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="20-Dec"~total_count/(16+5),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="20-Nov"~total_count/(143+32),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="21-Nov"~total_count/(131+24),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="21-Oct"~total_count/(124+37),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Default"&date=="22-Nov"~total_count/(70+9),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="22-Oct"~total_count/(132+26),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="23-Oct"~total_count/(124+28),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="24-Oct"~total_count/(131+32),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="25-Nov"~total_count/(55+20),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Control"&date=="25-Oct"~total_count/(80+14),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="26-Nov"~total_count/(29+6),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127+38),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(108+25),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="3-Dec"~total_count/(116+28),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(136+23),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(120+27),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="4-Dec"~total_count/(142+27),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(118+34),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="5-Dec"~total_count/(113+32),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(103+20),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="6-Dec"~total_count/(83+12),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(136+30),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(132+28),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(92+16),
                             item=="Create Your Pasta Bowl MEAT"&menu_condition=="Multimodal"&date=="9-Dec"~total_count/(108+32))) %>%
  drop_na(prop_high) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Create Your Pasta Bowl MEAT"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop_high,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  annotate("text",x=as.Date("2024-10-22"),y=0.8112435,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.8131534,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.8110291,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.7936117,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.8212435) +
  annotate("point",x=as.Date("2024-11-4"),y=0.8231534) +
  annotate("point",x=as.Date("2024-11-18"),y=0.8210291) +
  annotate("point",x=as.Date("2024-12-7"),y=0.8036117) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
fall_prop_high_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

``` r
fall_prop_high <- ggarrange(fall_prop_high_ramen,fall_prop_high_grill,fall_prop_high_treatment,fall_prop_high_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
fall_prop_high <- annotate_figure(fall_prop_high,top=text_grob("Daily Sales Percentages of Highest-Carbon Offerings (%)", 
               color="black",face="bold",size=12))
ggsave(filename="fall_prop_high.png",plot=fall_prop_high,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
fall_prop_high
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

## mean carbon

``` r
fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition [4]
    ##   menu_condition station total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>          <chr>               <dbl>       <int>            <dbl>
    ## 1 Carbon Label   Grill               3732.        1285             2.90
    ## 2 Control        Grill               3095.        1048             2.95
    ## 3 Default        Grill               3616.        1270             2.85
    ## 4 Multimodal     Grill               4623.        1595             2.90

``` r
fall_mean_carbon_grill <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Grill Station (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=2.912993,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=2.863895,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=2.806977,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=2.858700,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=2.952993) +
  annotate("point",x=as.Date("2024-11-4"),y=2.903895) +
  annotate("point",x=as.Date("2024-11-18"),y=2.846977) +
  annotate("point",x=as.Date("2024-12-7"),y=2.898700) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_carbon_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) 
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   menu_condition [4]
    ##   menu_condition station total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>          <chr>               <dbl>       <int>            <dbl>
    ## 1 Carbon Label   Ramen                301.         820            0.368
    ## 2 Control        Ramen                247.         671            0.368
    ## 3 Default        Ramen                318.         870            0.365
    ## 4 Multimodal     Ramen                382.        1042            0.367

``` r
fall_mean_carbon_ramen <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Ramen Station (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=0.3669079,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.3666386,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.3642188,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.3659046,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.3679079) +
  annotate("point",x=as.Date("2024-11-4"),y=0.3676386) +
  annotate("point",x=as.Date("2024-11-18"),y=0.3652188) +
  annotate("point",x=as.Date("2024-12-7"),y=0.3669046) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_carbon_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label               4033.        2105             1.92
    ## 2 Control                    3342.        1719             1.94
    ## 3 Default                    3933.        2140             1.84
    ## 4 Multimodal                 5006.        2637             1.90

``` r
fall_mean_carbon_treatment <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(station="Treatment") %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Ramen & Grill Stations (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=1.923922,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=1.895899,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=1.818038,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=1.878271,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=1.943922) +
  annotate("point",x=as.Date("2024-11-4"),y=1.915899) +
  annotate("point",x=as.Date("2024-11-18"),y=1.838038) +
  annotate("point",x=as.Date("2024-12-7"),y=1.898271) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date'. You can override using the
    ## `.groups` argument.

``` r
fall_mean_carbon_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>% 
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label                760.        1408            0.540
    ## 2 Control                     624.        1158            0.539
    ## 3 Default                     722.        1341            0.539
    ## 4 Multimodal                  705.        1329            0.530

``` r
fall_mean_carbon_control <- fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Pasta Station (Untreated)") +
  annotate("text",x=as.Date("2024-10-22"),y=0.5328448,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.5337717,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.5327407,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.5242881,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=0.5388448) +
  annotate("point",x=as.Date("2024-11-4"),y=0.5397717) +
  annotate("point",x=as.Date("2024-11-18"),y=0.5387407) +
  annotate("point",x=as.Date("2024-12-7"),y=0.5302881) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_carbon_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

``` r
fall_mean_carbon <- ggarrange(fall_mean_carbon_ramen,fall_mean_carbon_grill,fall_mean_carbon_treatment,fall_mean_carbon_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
fall_mean_carbon <- annotate_figure(fall_mean_carbon,top=text_grob("Mean Emissions Cost of Station Sales (kg CO2e)",color="black",face="bold",size=12))
ggsave(filename="fall_mean_carbon.png",plot=fall_mean_carbon,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
fall_mean_carbon
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

mean_spend

``` r
fall_data %>% 
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label              11554.        1285             8.99
    ## 2 Control                    9409.        1048             8.98
    ## 3 Default                   11421.        1270             8.99
    ## 4 Multimodal                14342.        1595             8.99

``` r
fall_mean_spend_grill <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Grill Station (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=8.978359-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=8.991089-0.01,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=8.992835-0.01,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=8.992006-0.01,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=8.978359) +
  annotate("point",x=as.Date("2024-11-4"),y=8.991089) +
  annotate("point",x=as.Date("2024-11-18"),y=8.992835) +
  annotate("point",x=as.Date("2024-12-7"),y=8.992006) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_spend_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

``` r
fall_data %>% 
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label               7782.         820             9.49
    ## 2 Control                    6368.         671             9.49
    ## 3 Default                    8256.         870             9.49
    ## 4 Multimodal                 9889.        1042             9.49

``` r
fall_mean_spend_ramen <- fall_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Ramen Station (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=9.49-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=9.49-0.01,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=9.49-0.01,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=9.49-0.01,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=9.49) +
  annotate("point",x=as.Date("2024-11-4"),y=9.49) +
  annotate("point",x=as.Date("2024-11-18"),y=9.49) +
  annotate("point",x=as.Date("2024-12-7"),y=9.49) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_spend_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-92-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label              19335.        2105             9.19
    ## 2 Control                   15777.        1719             9.18
    ## 3 Default                   19677.        2140             9.19
    ## 4 Multimodal                24231.        2637             9.19

``` r
fall_mean_spend_treatment <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(station="Treatment") %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Ramen & Grill Stations (Treated)") +
  annotate("text",x=as.Date("2024-10-22"),y=9.178074-0.005,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=9.185439-0.005,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=9.194953-0.005,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=9.188786-0.005,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=9.178074) +
  annotate("point",x=as.Date("2024-11-4"),y=9.185439) +
  annotate("point",x=as.Date("2024-11-18"),y=9.194953) +
  annotate("point",x=as.Date("2024-12-7"),y=9.188786) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date'. You can override using the
    ## `.groups` argument.

``` r
fall_mean_spend_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-94-1.png)<!-- -->

``` r
fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition) %>% 
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) 
```

    ## # A tibble: 4 × 4
    ##   menu_condition total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 Carbon Label              12533.        1408             8.90
    ## 2 Control                   10307.        1158             8.90
    ## 3 Default                   11936.        1341             8.90
    ## 4 Multimodal                11817.        1329             8.89

``` r
fall_mean_spend_control <- fall_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2024-10-16","2024-10-28","2024-11-11","2024-11-25","2024-12-20")),labels=c("Oct 16","Oct 28","Nov 11","Nov 25","Dec 20")) +
  labs(title="Pasta Station (Untreated)") +
  annotate("text",x=as.Date("2024-10-22"),y=8.900622-0.005,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=8.901577-0.005,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=8.900515-0.005,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=8.891806-0.005,label="Multimodal",size=10/.pt) +
  annotate("point",x=as.Date("2024-10-22"),y=8.900622) +
  annotate("point",x=as.Date("2024-11-4"),y=8.901577) +
  annotate("point",x=as.Date("2024-11-18"),y=8.900515) +
  annotate("point",x=as.Date("2024-12-7"),y=8.891806) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
fall_mean_spend_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-96-1.png)<!-- -->

``` r
fall_mean_spend <- ggarrange(fall_mean_spend_ramen,fall_mean_spend_grill,fall_mean_spend_treatment,fall_mean_spend_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
fall_mean_spend <- annotate_figure(fall_mean_spend,top=text_grob("Mean Revenue Per Station Sale ($)",color="black",face="bold",size=12))
ggsave(filename="fall_mean_spend.png",plot=fall_mean_spend,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
fall_mean_spend
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-97-1.png)<!-- -->

### Proportion of medium-emitting selections???? OMG Consider fixing code for prop-low and prop-high

menu_condition level

``` r
period_prop_middle_fall_data <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(item=="Black Bean Burger"&menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        item=="Grilled Hamburger"&menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        item=="Seared Salmon Burger"&menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Carbon Label"~total_count/(32+159+935+74+85),
                        item=="Black Bean Burger"&menu_condition=="Control"~total_count/(19+125+776+68+60),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Control"~total_count/(19+125+776+68+60),
                        item=="Grilled Hamburger"&menu_condition=="Control"~total_count/(19+125+776+68+60),
                        item=="Seared Salmon Burger"&menu_condition=="Control"~total_count/(19+125+776+68+60),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Control"~total_count/(19+125+776+68+60),
                        item=="Black Bean Burger"&menu_condition=="Default"~total_count/(33+167+904+76+90),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Default"~total_count/(33+167+904+76+90),
                        item=="Grilled Hamburger"&menu_condition=="Default"~total_count/(33+167+904+76+90),
                        item=="Seared Salmon Burger"&menu_condition=="Default"~total_count/(33+167+904+76+90),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Default"~total_count/(33+167+904+76+90),
                        item=="Black Bean Burger"&menu_condition=="Multimodal"~total_count/(41+165+1157+125+107),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Multimodal"~total_count/(41+165+1157+125+107),
                        item=="Grilled Hamburger"&menu_condition=="Multimodal"~total_count/(41+165+1157+125+107),
                        item=="Seared Salmon Burger"&menu_condition=="Multimodal"~total_count/(41+165+1157+125+107),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Multimodal"~total_count/(41+165+1157+125+107)))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

``` r
period_prop_middle_fall_data
```

    ## # A tibble: 20 × 4
    ## # Groups:   menu_condition [4]
    ##    menu_condition item                             total_count   prop
    ##    <chr>          <chr>                                  <int>  <dbl>
    ##  1 Carbon Label   Black Bean Burger                         32 0.0249
    ##  2 Carbon Label   Grilled Chicken Breast Sandwich          159 0.124 
    ##  3 Carbon Label   Grilled Hamburger                        935 0.728 
    ##  4 Carbon Label   Seared Salmon Burger                      74 0.0576
    ##  5 Carbon Label   Trillium Grill Impossible Burger          85 0.0661
    ##  6 Control        Black Bean Burger                         19 0.0181
    ##  7 Control        Grilled Chicken Breast Sandwich          125 0.119 
    ##  8 Control        Grilled Hamburger                        776 0.740 
    ##  9 Control        Seared Salmon Burger                      68 0.0649
    ## 10 Control        Trillium Grill Impossible Burger          60 0.0573
    ## 11 Default        Black Bean Burger                         33 0.0260
    ## 12 Default        Grilled Chicken Breast Sandwich          167 0.131 
    ## 13 Default        Grilled Hamburger                        904 0.712 
    ## 14 Default        Seared Salmon Burger                      76 0.0598
    ## 15 Default        Trillium Grill Impossible Burger          90 0.0709
    ## 16 Multimodal     Black Bean Burger                         41 0.0257
    ## 17 Multimodal     Grilled Chicken Breast Sandwich          165 0.103 
    ## 18 Multimodal     Grilled Hamburger                       1157 0.725 
    ## 19 Multimodal     Seared Salmon Burger                     125 0.0784
    ## 20 Multimodal     Trillium Grill Impossible Burger         107 0.0671

``` r
period_prop_middle_fall_data %>%
  filter(item=="Black Bean Burger") %>%
  group_by(menu_condition) %>%
  summarise(sum(prop))
```

    ## # A tibble: 4 × 2
    ##   menu_condition `sum(prop)`
    ##   <chr>                <dbl>
    ## 1 Carbon Label        0.0249
    ## 2 Control             0.0181
    ## 3 Default             0.0260
    ## 4 Multimodal          0.0257

``` r
period_prop_middle_fall_data %>%
  ggplot(aes(x=item,y=prop,fill=menu_condition)) +
  geom_col(position="dodge") 
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-100-1.png)<!-- -->

``` r
daily_prop_middle_fall_data <-fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                        menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                        menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                        menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                        menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                        menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                        menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                        menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                        menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                        menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                        menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                        menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                        menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                        menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                        menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                        menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                        menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                        menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                        menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                        menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                        menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                        menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                        menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                        menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                        menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                        menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                        menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                        menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                        menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                        menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                        menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                        menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                        menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                        menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                        menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                        menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                        menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                        menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                        menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                        menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                        menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                        menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                        menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                        menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                        menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
daily_prop_middle_fall_data <- daily_prop_middle_fall_data %>%
  filter(item=="Grilled Chicken Breast Sandwich"|item=="Seared Salmon Burger"|item=="Trillium Grill Impossible Burger") %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

``` r
grill_prop_mid <- ggplot(daily_prop_middle_fall_data,aes(x=date,y=prop,color=item)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  scale_color_brewer(palette="Set2") +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=0.17,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.17,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.17,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.17,label="Multimodal",size=10/.pt) 
grill_prop_mid
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-103-1.png)<!-- -->
ggsave(filename=“prop_middle.png”,plot=grill_prop_mid,path=“/Users/kenjinchang/github/multimodal-framework-validation/figures”,width=30,height=20,units=“cm”,dpi=150,limitsize=TRUE)

Now trying just prop of all items at grill

``` r
daily_prop_fall_data <-fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(menu_condition=="Carbon Label"&date=="1-Nov"~total_count/(94),
                        menu_condition=="Multimodal"&date=="10-Dec"~total_count/(72),
                        menu_condition=="Multimodal"&date=="11-Dec"~total_count/(68),
                        menu_condition=="Default"&date=="11-Nov"~total_count/(112),
                        menu_condition=="Multimodal"&date=="12-Dec"~total_count/(99),
                        menu_condition=="Default"&date=="12-Nov"~total_count/(134),
                        menu_condition=="Multimodal"&date=="13-Dec"~total_count/(66),
                        menu_condition=="Default"&date=="13-Nov"~total_count/(139),
                        menu_condition=="Default"&date=="14-Nov"~total_count/(145),
                        menu_condition=="Default"&date=="15-Nov"~total_count/(94),
                        menu_condition=="Multimodal"&date=="16-Dec"~total_count/(70),
                        menu_condition=="Control"&date=="16-Oct"~total_count/(123),
                        menu_condition=="Multimodal"&date=="17-Dec"~total_count/(87),
                        menu_condition=="Control"&date=="17-Oct"~total_count/(153),
                        menu_condition=="Multimodal"&date=="18-Dec"~total_count/(71),
                        menu_condition=="Default"&date=="18-Nov"~total_count/(130),
                        menu_condition=="Control"&date=="18-Oct"~total_count/(89),
                        menu_condition=="Multimodal"&date=="19-Dec"~total_count/(50),
                        menu_condition=="Default"&date=="19-Nov"~total_count/(167),
                        menu_condition=="Multimodal"&date=="2-Dec"~total_count/(118),
                        menu_condition=="Multimodal"&date=="20-Dec"~total_count/(39),
                        menu_condition=="Default"&date=="20-Nov"~total_count/(129),
                        menu_condition=="Default"&date=="21-Nov"~total_count/(144),
                        menu_condition=="Control"&date=="21-Oct"~total_count/(143),
                        menu_condition=="Default"&date=="22-Nov"~total_count/(76),
                        menu_condition=="Control"&date=="22-Oct"~total_count/(163),
                        menu_condition=="Control"&date=="23-Oct"~total_count/(135),
                        menu_condition=="Control"&date=="24-Oct"~total_count/(145),
                        menu_condition=="Multimodal"&date=="25-Nov"~total_count/(98),
                        menu_condition=="Control"&date=="25-Oct"~total_count/(97),
                        menu_condition=="Multimodal"&date=="26-Nov"~total_count/(75),
                        menu_condition=="Carbon Label"&date=="28-Oct"~total_count/(127),
                        menu_condition=="Carbon Label"&date=="29-Oct"~total_count/(145),
                        menu_condition=="Multimodal"&date=="3-Dec"~total_count/(131),
                        menu_condition=="Carbon Label"&date=="30-Oct"~total_count/(132),
                        menu_condition=="Carbon Label"&date=="31-Oct"~total_count/(149),
                        menu_condition=="Multimodal"&date=="4-Dec"~total_count/(146),
                        menu_condition=="Carbon Label"&date=="4-Nov"~total_count/(114),
                        menu_condition=="Multimodal"&date=="5-Dec"~total_count/(173),
                        menu_condition=="Carbon Label"&date=="5-Nov"~total_count/(138),
                        menu_condition=="Multimodal"&date=="6-Dec"~total_count/(103),
                        menu_condition=="Carbon Label"&date=="6-Nov"~total_count/(137),
                        menu_condition=="Carbon Label"&date=="7-Nov"~total_count/(142),
                        menu_condition=="Carbon Label"&date=="8-Nov"~total_count/(107),
                        menu_condition=="Multimodal"&date=="9-Dec"~total_count/(128))) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
ggplot(daily_prop_fall_data,aes(x=date,y=prop,color=item)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-105-1.png)<!-- -->

### Mean carbon costs

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station) %>%
  summarise(tot_carbon_cost=sum(corr_carbon_cost),tot_water_cost=sum(corr_water_cost),tot_dollar_cost=sum(corr_dollar_cost),tot_sales=sum(count)) %>%
  mutate(mean_carbon_cost=tot_carbon_cost/tot_sales) %>%
  mutate(mean_water_cost=tot_water_cost/tot_sales) %>%
  mutate(mean_spend=tot_dollar_cost/tot_sales) %>%
  filter(station=="Grill") %>%
  select(menu_condition,mean_carbon_cost,mean_spend)
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   menu_condition [4]
    ##   menu_condition mean_carbon_cost mean_spend
    ##   <chr>                     <dbl>      <dbl>
    ## 1 Carbon Label               2.90       8.99
    ## 2 Control                    2.95       8.98
    ## 3 Default                    2.85       8.99
    ## 4 Multimodal                 2.90       8.99

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station) %>%
  summarise(tot_carbon_cost=sum(corr_carbon_cost),tot_water_cost=sum(corr_water_cost),tot_dollar_cost=sum(corr_dollar_cost),tot_sales=sum(count)) %>%
  mutate(mean_carbon_cost=tot_carbon_cost/tot_sales) %>%
  mutate(mean_water_cost=tot_water_cost/tot_sales) %>%
  mutate(mean_spend=tot_dollar_cost/tot_sales) %>%
  ggplot(aes(x=menu_condition,y=mean_carbon_cost,fill=station)) + 
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-107-1.png)<!-- -->

``` r
mean_per_day_carbon_cost_fall_data <- fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
ggplot(mean_per_day_carbon_cost_fall_data,aes(x=date,y=mean_carbon_cost,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->

``` r
grill_mean_carbon_cost <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station)) + 
  geom_smooth() + 
  geom_point() +
  xlab("Date") +
  ylab(bquote('Mean Emissions Cost (kg CO'[2]*'e)')) +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=3.15,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=3.15,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=3.15,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=3.15,label="Multimodal",size=10/.pt) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
grill_mean_carbon_cost
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-110-1.png)<!-- -->
ggsave(filename=“mean_carbon.png”,plot=grill_mean_carbon_cost,path=“/Users/kenjinchang/github/multimodal-framework-validation/figures”,width=30,height=20,units=“cm”,dpi=150,limitsize=TRUE)

### Mean spend

``` r
fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station) %>%
  summarise(tot_carbon_cost=sum(corr_carbon_cost),tot_water_cost=sum(corr_water_cost),tot_dollar_cost=sum(corr_dollar_cost),tot_sales=sum(count)) %>%
  mutate(mean_carbon_cost=tot_carbon_cost/tot_sales) %>%
  mutate(mean_water_cost=tot_water_cost/tot_sales) %>%
  mutate(mean_spend=tot_dollar_cost/tot_sales) %>%
  ggplot(aes(x=menu_condition,y=mean_spend,fill=station)) + 
  geom_col(position="dodge") + 
  scale_x_discrete(limits=c("Control","Carbon Label","Default","Multimodal"))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-111-1.png)<!-- -->

``` r
mean_daily_spend_fall_data <- fall_data %>%
  filter(station_type=="Treatment") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_spend=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_spend=total_spend/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
ggplot(mean_daily_spend_fall_data,aes(x=date,y=mean_spend,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-113-1.png)<!-- -->

``` r
mean_spend <- fall_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_spend=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_spend=total_spend/total_sales) %>%
  mutate(date=case_when(date=="16-Oct"~"2024-10-16",
                        date=="17-Oct"~"2024-10-17",
                        date=="18-Oct"~"2024-10-18",
                        date=="21-Oct"~"2024-10-21",
                        date=="22-Oct"~"2024-10-22",
                        date=="23-Oct"~"2024-10-23",
                        date=="24-Oct"~"2024-10-24",
                        date=="25-Oct"~"2024-10-25",
                        date=="28-Oct"~"2024-10-28",
                        date=="29-Oct"~"2024-10-29",
                        date=="30-Oct"~"2024-10-30",
                        date=="31-Oct"~"2024-10-31",
                        date=="1-Nov"~"2024-11-1",
                        date=="4-Nov"~"2024-11-4",
                        date=="5-Nov"~"2024-11-5",
                        date=="6-Nov"~"2024-11-6",
                        date=="7-Nov"~"2024-11-7",
                        date=="8-Nov"~"2024-11-8",
                        date=="11-Nov"~"2024-11-11",
                        date=="12-Nov"~"2024-11-12",
                        date=="13-Nov"~"2024-11-13",
                        date=="14-Nov"~"2024-11-14",
                        date=="15-Nov"~"2024-11-15",
                        date=="18-Nov"~"2024-11-18",
                        date=="19-Nov"~"2024-11-19",
                        date=="20-Nov"~"2024-11-20",
                        date=="21-Nov"~"2024-11-21",
                        date=="22-Nov"~"2024-11-22",
                        date=="25-Nov"~"2024-11-25",
                        date=="26-Nov"~"2024-11-26",
                        date=="2-Dec"~"2024-12-2",
                        date=="3-Dec"~"2024-12-3",
                        date=="4-Dec"~"2024-12-4",
                        date=="5-Dec"~"2024-12-5",
                        date=="6-Dec"~"2024-12-6",
                        date=="9-Dec"~"2024-12-9",
                        date=="10-Dec"~"2024-12-10",
                        date=="11-Dec"~"2024-12-11",
                        date=="12-Dec"~"2024-12-12",
                        date=="13-Dec"~"2024-12-13",
                        date=="16-Dec"~"2024-12-16",
                        date=="17-Dec"~"2024-12-17",
                        date=="18-Dec"~"2024-12-18",
                        date=="19-Dec"~"2024-12-19",
                        date=="20-Dec"~"2024-12-20")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_spend,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Mean Spend ($)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=9.06,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=9.06,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=9.06,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=9.06,label="Multimodal",size=10/.pt) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
mean_spend
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-114-1.png)<!-- -->
ggsave(filename=“mean_spend.png”,plot=mean_spend,path=“/Users/kenjinchang/github/multimodal-framework-validation/figures”,width=30,height=20,units=“cm”,dpi=150,limitsize=TRUE)

## CLeaning and Analysis (Spring 2025)

### Proportion of lowest-carbon selections

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  mutate(phase_interval=case_when(date=="21-Jan"~"1",
                                  date=="22-Jan"~"1",
                                  date=="23-Jan"~"1",
                                  date=="24-Jan"~"1",
                                  date=="27-Jan"~"1",
                                  date=="28-Jan"~"1",
                                  date=="29-Jan"~"1",
                                  date=="30-Jan"~"1",
                                  date=="31-Jan"~"1",
                                  date=="3-Feb"~"2",
                                  date=="4-Feb"~"2",
                                  date=="5-Feb"~"2",
                                  date=="6-Feb"~"2",
                                  date=="7-Feb"~"2",
                                  date=="10-Feb"~"2",
                                  date=="11-Feb"~"2",
                                  date=="12-Feb"~"2",
                                  date=="13-Feb"~"2",
                                  date=="14-Feb"~"2",
                                  date=="19-Feb"~"2",
                                  date=="20-Feb"~"2",
                                  date=="21-Feb"~"2",
                                  date=="24-Feb"~"3",
                                  date=="25-Feb"~"3",
                                  date=="26-Feb"~"3",
                                  date=="27-Feb"~"3",
                                  date=="28-Feb"~"3",
                                  date=="3-Mar"~"3",
                                  date=="4-Mar"~"3",
                                  date=="5-Mar"~"3",
                                  date=="6-Mar"~"3",
                                  date=="7-Mar"~"3",
                                  date=="10-Mar"~"4",
                                  date=="11-Mar"~"4",
                                  date=="12-Mar"~"4",
                                  date=="13-Mar"~"4",
                                  date=="14-Mar"~"4",
                                  date=="17-Mar"~"4",
                                  date=="18-Mar"~"4",
                                  date=="19-Mar"~"4",
                                  date=="20-Mar"~"4",
                                  date=="21-Mar"~"4",
                                  date=="24-Mar"~"5",
                                  date=="25-Mar"~"5",
                                  date=="26-Mar"~"5",
                                  date=="27-Mar"~"5",
                                  date=="28-Mar"~"5",
                                  date=="7-Apr"~"5",
                                  date=="8-Apr"~"5",
                                  date=="9-Apr"~"5",
                                  date=="10-Apr"~"5",
                                  date=="11-Apr"~"5",
                                  date=="14-Apr"~"5",
                                  date=="15-Apr"~"5",
                                  date=="16-Apr"~"5",
                                  date=="17-Apr"~"5",
                                  date=="18-Apr"~"5")) %>%
  group_by(phase_interval,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(item=="Black Bean Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Grilled Hamburger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Seared Salmon Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Black Bean Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Grilled Hamburger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Seared Salmon Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Black Bean Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Grilled Hamburger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Seared Salmon Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Black Bean Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Grilled Hamburger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Seared Salmon Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Black Bean Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Grilled Hamburger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Seared Salmon Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125))) %>%
  ggplot(aes(x=item,y=prop,fill=phase_interval)) + 
  geom_col(position="dodge")
```

    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-115-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  mutate(phase_interval=case_when(date=="21-Jan"~"1",
                                  date=="22-Jan"~"1",
                                  date=="23-Jan"~"1",
                                  date=="24-Jan"~"1",
                                  date=="27-Jan"~"1",
                                  date=="28-Jan"~"1",
                                  date=="29-Jan"~"1",
                                  date=="30-Jan"~"1",
                                  date=="31-Jan"~"1",
                                  date=="3-Feb"~"2",
                                  date=="4-Feb"~"2",
                                  date=="5-Feb"~"2",
                                  date=="6-Feb"~"2",
                                  date=="7-Feb"~"2",
                                  date=="10-Feb"~"2",
                                  date=="11-Feb"~"2",
                                  date=="12-Feb"~"2",
                                  date=="13-Feb"~"2",
                                  date=="14-Feb"~"2",
                                  date=="19-Feb"~"2",
                                  date=="20-Feb"~"2",
                                  date=="21-Feb"~"2",
                                  date=="24-Feb"~"3",
                                  date=="25-Feb"~"3",
                                  date=="26-Feb"~"3",
                                  date=="27-Feb"~"3",
                                  date=="28-Feb"~"3",
                                  date=="3-Mar"~"3",
                                  date=="4-Mar"~"3",
                                  date=="5-Mar"~"3",
                                  date=="6-Mar"~"3",
                                  date=="7-Mar"~"3",
                                  date=="10-Mar"~"4",
                                  date=="11-Mar"~"4",
                                  date=="12-Mar"~"4",
                                  date=="13-Mar"~"4",
                                  date=="14-Mar"~"4",
                                  date=="17-Mar"~"4",
                                  date=="18-Mar"~"4",
                                  date=="19-Mar"~"4",
                                  date=="20-Mar"~"4",
                                  date=="21-Mar"~"4",
                                  date=="24-Mar"~"5",
                                  date=="25-Mar"~"5",
                                  date=="26-Mar"~"5",
                                  date=="27-Mar"~"5",
                                  date=="28-Mar"~"5",
                                  date=="7-Apr"~"5",
                                  date=="8-Apr"~"5",
                                  date=="9-Apr"~"5",
                                  date=="10-Apr"~"5",
                                  date=="11-Apr"~"5",
                                  date=="14-Apr"~"5",
                                  date=="15-Apr"~"5",
                                  date=="16-Apr"~"5",
                                  date=="17-Apr"~"5",
                                  date=="18-Apr"~"5")) %>%
  group_by(phase_interval,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(item=="Black Bean Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Grilled Hamburger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Seared Salmon Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="1"~total_count/(25+135+814+72+76),
                        item=="Black Bean Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Grilled Hamburger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Seared Salmon Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="2"~total_count/(30+193+1083+97+74),
                        item=="Black Bean Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Grilled Hamburger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Seared Salmon Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="3"~total_count/(18+167+938+87+66),
                        item=="Black Bean Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Grilled Hamburger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Seared Salmon Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="4"~total_count/(33+160+909+67+74),
                        item=="Black Bean Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Grilled Chicken Breast Sandwich"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Grilled Hamburger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Seared Salmon Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125),
                        item=="Trillium Grill Impossible Burger"&phase_interval=="5"~total_count/(46+250+1352+130+125))) %>%
  filter(item=="Grilled Hamburger") 
```

    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   phase_interval [5]
    ##   phase_interval item              total_count  prop
    ##   <chr>          <chr>                   <int> <dbl>
    ## 1 1              Grilled Hamburger         814 0.725
    ## 2 2              Grilled Hamburger        1083 0.733
    ## 3 3              Grilled Hamburger         938 0.735
    ## 4 4              Grilled Hamburger         909 0.731
    ## 5 5              Grilled Hamburger        1352 0.710

mutate(prop=case_when(item==““&phase_interval==”“~))

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(item=="Black Bean Burger"&menu_condition=="Control"~total_count/(89+552+3104+289+267),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Control"~total_count/(89+552+3104+289+267),
                        item=="Grilled Hamburger"&menu_condition=="Control"~total_count/(89+552+3104+289+267),
                        item=="Seared Salmon Burger"&menu_condition=="Control"~total_count/(89+552+3104+289+267),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Control"~total_count/(89+552+3104+289+267),
                        item=="Black Bean Burger"&menu_condition=="Multimodal"~total_count/(30+193+1083+97+74),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Multimodal"~total_count/(30+193+1083+97+74),
                        item=="Grilled Hamburger"&menu_condition=="Multimodal"~total_count/(30+193+1083+97+74),
                        item=="Seared Salmon Burger"&menu_condition=="Multimodal"~total_count/(30+193+1083+97+74),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Multimodal"~total_count/(30+193+1083+97+74),
                        item=="Black Bean Burger"&menu_condition=="Unimodal"~total_count/(33+160+909+67+74),
                        item=="Grilled Chicken Breast Sandwich"&menu_condition=="Unimodal"~total_count/(33+160+909+67+74),
                        item=="Grilled Hamburger"&menu_condition=="Unimodal"~total_count/(33+160+909+67+74),
                        item=="Seared Salmon Burger"&menu_condition=="Unimodal"~total_count/(33+160+909+67+74),
                        item=="Trillium Grill Impossible Burger"&menu_condition=="Unimodal"~total_count/(33+160+909+67+74))) 
```

    ## `summarise()` has grouped output by 'menu_condition', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 15 × 5
    ## # Groups:   menu_condition, station [3]
    ##    menu_condition station item                             total_count   prop
    ##    <chr>          <chr>   <chr>                                  <int>  <dbl>
    ##  1 Control        Grill   Black Bean Burger                         89 0.0207
    ##  2 Control        Grill   Grilled Chicken Breast Sandwich          552 0.128 
    ##  3 Control        Grill   Grilled Hamburger                       3104 0.722 
    ##  4 Control        Grill   Seared Salmon Burger                     289 0.0672
    ##  5 Control        Grill   Trillium Grill Impossible Burger         267 0.0621
    ##  6 Multimodal     Grill   Black Bean Burger                         30 0.0203
    ##  7 Multimodal     Grill   Grilled Chicken Breast Sandwich          193 0.131 
    ##  8 Multimodal     Grill   Grilled Hamburger                       1083 0.733 
    ##  9 Multimodal     Grill   Seared Salmon Burger                      97 0.0657
    ## 10 Multimodal     Grill   Trillium Grill Impossible Burger          74 0.0501
    ## 11 Unimodal       Grill   Black Bean Burger                         33 0.0265
    ## 12 Unimodal       Grill   Grilled Chicken Breast Sandwich          160 0.129 
    ## 13 Unimodal       Grill   Grilled Hamburger                        909 0.731 
    ## 14 Unimodal       Grill   Seared Salmon Burger                      67 0.0539
    ## 15 Unimodal       Grill   Trillium Grill Impossible Burger          74 0.0595

period level

``` r
spring_data <- spring_data %>%
  mutate(phase_interval=case_when(date=="21-Jan"~"1",
                                  date=="22-Jan"~"1",
                                  date=="23-Jan"~"1",
                                  date=="24-Jan"~"1",
                                  date=="27-Jan"~"1",
                                  date=="28-Jan"~"1",
                                  date=="29-Jan"~"1",
                                  date=="30-Jan"~"1",
                                  date=="31-Jan"~"1",
                                  date=="3-Feb"~"2",
                                  date=="4-Feb"~"2",
                                  date=="5-Feb"~"2",
                                  date=="6-Feb"~"2",
                                  date=="7-Feb"~"2",
                                  date=="10-Feb"~"2",
                                  date=="11-Feb"~"2",
                                  date=="12-Feb"~"2",
                                  date=="13-Feb"~"2",
                                  date=="14-Feb"~"2",
                                  date=="19-Feb"~"2",
                                  date=="20-Feb"~"2",
                                  date=="21-Feb"~"2",
                                  date=="24-Feb"~"3",
                                  date=="25-Feb"~"3",
                                  date=="26-Feb"~"3",
                                  date=="27-Feb"~"3",
                                  date=="28-Feb"~"3",
                                  date=="3-Mar"~"3",
                                  date=="4-Mar"~"3",
                                  date=="5-Mar"~"3",
                                  date=="6-Mar"~"3",
                                  date=="7-Mar"~"3",
                                  date=="10-Mar"~"4",
                                  date=="11-Mar"~"4",
                                  date=="12-Mar"~"4",
                                  date=="13-Mar"~"4",
                                  date=="14-Mar"~"4",
                                  date=="17-Mar"~"4",
                                  date=="18-Mar"~"4",
                                  date=="19-Mar"~"4",
                                  date=="20-Mar"~"4",
                                  date=="21-Mar"~"4",
                                  date=="24-Mar"~"5",
                                  date=="25-Mar"~"5",
                                  date=="26-Mar"~"5",
                                  date=="27-Mar"~"5",
                                  date=="28-Mar"~"5",
                                  date=="7-Apr"~"5",
                                  date=="8-Apr"~"5",
                                  date=="9-Apr"~"5",
                                  date=="10-Apr"~"5",
                                  date=="11-Apr"~"5",
                                  date=="14-Apr"~"5",
                                  date=="15-Apr"~"5",
                                  date=="16-Apr"~"5",
                                  date=="17-Apr"~"5",
                                  date=="18-Apr"~"5")) 
```

fall_data %\>% filter(station_type==“Treatment”) %\>%
filter(item_cat==“Main”) %\>% group_by(menu_condition,station) %\>%
summarise(tot_carbon_cost=sum(corr_carbon_cost),tot_water_cost=sum(corr_water_cost),tot_dollar_cost=sum(corr_dollar_cost),tot_sales=sum(count))
%\>% mutate(mean_carbon_cost=tot_carbon_cost/tot_sales) %\>%
mutate(mean_water_cost=tot_water_cost/tot_sales) %\>%
mutate(mean_spend=tot_dollar_cost/tot_sales) %\>%
filter(station==“Grill”) %\>%
select(menu_condition,mean_carbon_cost,mean_spend)

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>%
  summarise(tot_carbon_cost=sum(corr_carbon_cost),tot_dollar_cost=sum(corr_dollar_cost),tot_sales=sum(count)) %>%
  mutate(mean_carbon_cost=tot_carbon_cost/tot_sales) %>%
  mutate(mean_spend=tot_dollar_cost/tot_sales) %>%
  select(phase_interval,mean_carbon_cost,mean_spend)
```

    ## # A tibble: 5 × 3
    ##   phase_interval mean_carbon_cost mean_spend
    ##   <chr>                     <dbl>      <dbl>
    ## 1 1                          2.90       8.99
    ## 2 2                          2.93       8.96
    ## 3 3                          2.94       8.96
    ## 4 4                          2.92       8.98
    ## 5 5                          2.84       8.98

daily

``` r
daily_prop_spring_data <- spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(6+17+90+10+8),
                        date=="10-Feb"~total_count/(4+14+101+14+4),
                        date=="10-Mar"~total_count/(3+13+86+8+7),
                        date=="11-Apr"~total_count/(1+9+72+7+14),
                        date=="11-Feb"~total_count/(3+26+86+8+6),
                        date=="11-Mar"~total_count/(3+20+118+5+6),
                        date=="12-Feb"~total_count/(3+21+101+12+7),
                        date=="12-Mar"~total_count/(6+19+89+7+7),
                        date=="13-Feb"~total_count/(3+12+83+11+10),
                        date=="13-Mar"~total_count/(17+99+8+5),
                        date=="14-Apr"~total_count/(3+19+79+10+14),
                        date=="14-Feb"~total_count/(2+5+72+5+4),
                        date=="14-Mar"~total_count/(1+11+76+4+10),
                        date=="15-Apr"~total_count/(2+20+101+10+8),
                        date=="16-Apr"~total_count/(3+13+103+9+9),
                        date=="17-Apr"~total_count/(2+16+113+5+13),
                        date=="17-Mar"~total_count/(8+17+77+10+12),
                        date=="18-Apr"~total_count/(2+6+76+10+3),
                        date=="18-Mar"~total_count/(3+16+80+6+7),
                        date=="19-Feb"~total_count/(1+17+84+5+3),
                        date=="19-Mar"~total_count/(5+20+96+6+3),
                        date=="20-Feb"~total_count/(4+17+102+5+10),
                        date=="20-Mar"~total_count/(3+18+103+6+12),
                        date=="21-Feb"~total_count/(2+17+73+8+3),
                        date=="21-Jan"~total_count/(2+17+94+6+8),
                        date=="21-Mar"~total_count/(1+9+85+7+5),
                        date=="22-Jan"~total_count/(3+14+105+9+11),
                        date=="23-Jan"~total_count/(2+15+94+11+9),
                        date=="24-Feb"~total_count/(1+15+95+14+6),
                        date=="24-Jan"~total_count/(1+13+63+5+7),
                        date=="24-Mar"~total_count/(2+23+81+15+13),
                        date=="25-Feb"~total_count/(4+20+102+6+8),
                        date=="25-Mar"~total_count/(5+15+122+8+6),
                        date=="26-Feb"~total_count/(1+28+90+15+3),
                        date=="26-Mar"~total_count/(7+19+108+6+5),
                        date=="27-Feb"~total_count/(2+17+105+13+8),
                        date=="27-Jan"~total_count/(4+22+92+10+12),
                        date=="27-Mar"~total_count/(1+10+99+10+7),
                        date=="28-Feb"~total_count/(1+14+72+9+2),
                        date=="28-Jan"~total_count/(5+19+105+7+10),
                        date=="28-Mar"~total_count/(5+8+29+5+3),
                        date=="29-Jan"~total_count/(4+12+88+11+5),
                        date=="3-Feb"~total_count/(15+90+8+12),
                        date=="3-Mar"~total_count/(2+16+95+9+12),
                        date=="30-Jan"~total_count/(3+14+109+8+8),
                        date=="31-Jan"~total_count/(1+9+64+5+6),
                        date=="4-Feb"~total_count/(3+19+108+7+2),
                        date=="4-Mar"~total_count/(2+21+104+10+8),
                        date=="5-Feb"~total_count/(3+23+97+6+7),
                        date=="5-Mar"~total_count/(3+16+86+4+6),
                        date=="6-Mar"~total_count/(1+12+114+3+8),
                        date=="7-Apr"~total_count/(4+17+77+4+3),
                        date=="7-Feb"~total_count/(2+7+86+8+6),
                        date=="7-Mar"~total_count/(1+8+75+4+5),
                        date=="8-Apr"~total_count/(2+28+113+11+13),
                        date=="9-Apr"~total_count/(1+30+89+10+6))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
daily_prop_spring_data
```

    ## # A tibble: 278 × 5
    ## # Groups:   date, menu_condition [56]
    ##    date       menu_condition item                             total_count   prop
    ##    <date>     <chr>          <chr>                                  <int>  <dbl>
    ##  1 2025-04-10 Control        Black Bean Burger                          6 0.0458
    ##  2 2025-04-10 Control        Grilled Chicken Breast Sandwich           17 0.130 
    ##  3 2025-04-10 Control        Grilled Hamburger                         90 0.687 
    ##  4 2025-04-10 Control        Seared Salmon Burger                      10 0.0763
    ##  5 2025-04-10 Control        Trillium Grill Impossible Burger           8 0.0611
    ##  6 2025-02-10 Multimodal     Black Bean Burger                          4 0.0292
    ##  7 2025-02-10 Multimodal     Grilled Chicken Breast Sandwich           14 0.102 
    ##  8 2025-02-10 Multimodal     Grilled Hamburger                        101 0.737 
    ##  9 2025-02-10 Multimodal     Seared Salmon Burger                      14 0.102 
    ## 10 2025-02-10 Multimodal     Trillium Grill Impossible Burger           4 0.0292
    ## # ℹ 268 more rows

``` r
spring_vline <- as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")) 
spring_vline <- which(daily_prop_spring_data$date %in% spring_vline)
```

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(25+135+814+72+76),
                        phase_interval=="2"~total_count/(30+193+1083+97+74),
                        phase_interval=="3"~total_count/(18+167+938+87+66),
                        phase_interval=="4"~total_count/(33+160+909+67+74),
                        phase_interval=="5"~total_count/(45+250+1352+130+125))) %>%
  filter(item=="Black Bean Burger")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item              total_count   prop
    ##   <chr>          <chr>   <chr>                   <int>  <dbl>
    ## 1 1              Grill   Black Bean Burger          25 0.0223
    ## 2 2              Grill   Black Bean Burger          30 0.0203
    ## 3 3              Grill   Black Bean Burger          18 0.0141
    ## 4 4              Grill   Black Bean Burger          33 0.0265
    ## 5 5              Grill   Black Bean Burger          46 0.0242

``` r
spring_prop_low_grill <- daily_prop_spring_data %>%
  filter(item=="Black Bean Burger") %>%
  mutate(item=case_when(item=="Black Bean Burger"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.01828164,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.01631144,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.01010658,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.02254867,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.02018507,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.02228164) +
  annotate("point",x=as.Date("2025-02-14"),y=0.02031144) +
  annotate("point",x=as.Date("2025-03-03"),y=0.01410658) +
  annotate("point",x=as.Date("2025-03-17"),y=0.02654867) +
  annotate("point",x=as.Date("2025-04-06"),y=0.02418507) +
  labs(title="Grill Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
spring_prop_low_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-123-1.png)<!-- -->

FOR RAMEN STATION

``` r
spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(740+152),
                        phase_interval=="2"~total_count/(928+213),
                        phase_interval=="3"~total_count/(726+176),
                        phase_interval=="4"~total_count/(602+163),
                        phase_interval=="5"~total_count/(890+262))) %>%
  filter(item=="Bowl Ramen Tofu")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item            total_count  prop
    ##   <chr>          <chr>   <chr>                 <int> <dbl>
    ## 1 1              Ramen   Bowl Ramen Tofu         152 0.170
    ## 2 2              Ramen   Bowl Ramen Tofu         213 0.187
    ## 3 3              Ramen   Bowl Ramen Tofu         176 0.195
    ## 4 4              Ramen   Bowl Ramen Tofu         163 0.213
    ## 5 5              Ramen   Bowl Ramen Tofu         262 0.227

``` r
spring_prop_low_ramen <- spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(80+24),
                        date=="10-Feb"~total_count/(58+21),
                        date=="10-Mar"~total_count/(38+6),
                        date=="11-Apr"~total_count/(46+8),
                        date=="11-Feb"~total_count/(95+20),
                        date=="11-Mar"~total_count/(51+11),
                        date=="12-Feb"~total_count/(79+11),
                        date=="12-Mar"~total_count/(72+17),
                        date=="13-Feb"~total_count/(66+21),
                        date=="13-Mar"~total_count/(75+24),
                        date=="14-Apr"~total_count/(38+27),
                        date=="14-Feb"~total_count/(43+15),
                        date=="14-Mar"~total_count/(47+11),
                        date=="15-Apr"~total_count/(71+28),
                        date=="16-Apr"~total_count/(61+13),
                        date=="17-Apr"~total_count/(58+24),
                        date=="17-Mar"~total_count/(84+19),
                        date=="18-Apr"~total_count/(46+12),
                        date=="18-Mar"~total_count/(74+23),
                        date=="19-Feb"~total_count/(81+15),
                        date=="19-Mar"~total_count/(62+15),
                        date=="20-Feb"~total_count/(62+15),
                        date=="20-Mar"~total_count/(59+27),
                        date=="21-Feb"~total_count/(58+14),
                        date=="21-Jan"~total_count/(83+19),
                        date=="21-Mar"~total_count/(40+10),
                        date=="22-Jan"~total_count/(90+12),
                        date=="23-Jan"~total_count/(92+18),
                        date=="24-Feb"~total_count/(79+14),
                        date=="24-Jan"~total_count/(67+11),
                        date=="24-Mar"~total_count/(54+13),
                        date=="25-Feb"~total_count/(101+24),
                        date=="25-Mar"~total_count/(86+19),
                        date=="26-Feb"~total_count/(63+12),
                        date=="26-Mar"~total_count/(52+16),
                        date=="27-Feb"~total_count/(85+18),
                        date=="27-Jan"~total_count/(80+11),
                        date=="27-Mar"~total_count/(61+20),
                        date=="28-Feb"~total_count/(49+11),
                        date=="28-Jan"~total_count/(88+21),
                        date=="28-Mar"~total_count/(33+6),
                        date=="29-Jan"~total_count/(85+21),
                        date=="3-Feb"~total_count/(81+12),
                        date=="3-Mar"~total_count/(72+17),
                        date=="30-Jan"~total_count/(83+26),
                        date=="31-Jan"~total_count/(72+13),
                        date=="4-Feb"~total_count/(86+25),
                        date=="4-Mar"~total_count/(75+20),
                        date=="5-Feb"~total_count/(78+14),
                        date=="5-Mar"~total_count/(72+20),
                        date=="6-Feb"~total_count/(98+12),
                        date=="6-Mar"~total_count/(73+24),
                        date=="7-Apr"~total_count/(51+17),
                        date=="7-Feb"~total_count/(43+18),
                        date=="7-Mar"~total_count/(57+16),
                        date=="8-Apr"~total_count/(84+21),
                        date=="9-Apr"~total_count/(69+14))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Bowl Ramen Tofu") %>% ### Lowest-carbon offering
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.1504036,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.1666784,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.1751220,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.1930719,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.2074306,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.1704036) +
  annotate("point",x=as.Date("2025-02-14"),y=0.1866784) +
  annotate("point",x=as.Date("2025-03-03"),y=0.1951220) +
  annotate("point",x=as.Date("2025-03-17"),y=0.2130719) +
  annotate("point",x=as.Date("2025-04-06"),y=0.2274306) +
  labs(title="Ramen Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval', 'station'. You
    ## can override using the `.groups` argument.

``` r
spring_prop_low_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-125-1.png)<!-- -->

FOR AGGREGATE

``` r
spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count))  %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(25+135+814+72+76+740+152),
                        phase_interval=="2"~total_count/(30+193+1083+97+74+928+213),
                        phase_interval=="3"~total_count/(18+167+938+87+66+726+176),
                        phase_interval=="4"~total_count/(33+160+909+67+74+602+163),
                        phase_interval=="5"~total_count/(46+250+1352+130+125+890+262))) %>%
   mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(phase_interval,item) %>%
  summarise(prop=sum(prop)) %>%
  filter(item=="Lowest-Carbon Offering")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   phase_interval [5]
    ##   phase_interval item                     prop
    ##   <chr>          <chr>                   <dbl>
    ## 1 1              Lowest-Carbon Offering 0.0879
    ## 2 2              Lowest-Carbon Offering 0.0928
    ## 3 3              Lowest-Carbon Offering 0.0891
    ## 4 4              Lowest-Carbon Offering 0.0976
    ## 5 5              Lowest-Carbon Offering 0.101

``` r
spring_prop_low_treatment <- spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"High",
                        item=="Grilled Hamburger"~"High",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(date,phase_interval,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(170+30+35),
                        date=="10-Feb"~total_count/(159+25+32),
                        date=="10-Mar"~total_count/(124+9+28),
                        date=="11-Apr"~total_count/(118+9+30),
                        date=="11-Feb"~total_count/(181+23+40),
                        date=="11-Mar"~total_count/(169+14+31),
                        date=="12-Feb"~total_count/(180+14+40),
                        date=="12-Mar"~total_count/(161+23+33),
                        date=="13-Feb"~total_count/(149+24+33),
                        date=="13-Mar"~total_count/(174+24+30),
                        date=="14-Apr"~total_count/(117+30+43),
                        date=="14-Feb"~total_count/(115+17+14),
                        date=="14-Mar"~total_count/(123+12+25),
                        date=="15-Apr"~total_count/(172+30+38),
                        date=="16-Apr"~total_count/(164+16+31),
                        date=="17-Apr"~total_count/(171+26+34),
                        date=="17-Mar"~total_count/(161+27+39),
                        date=="18-Apr"~total_count/(122+14+19),
                        date=="18-Mar"~total_count/(154+26+29),
                        date=="19-Feb"~total_count/(165+16+25),
                        date=="19-Mar"~total_count/(158+20+29),
                        date=="20-Feb"~total_count/(164+19+32),
                        date=="20-Mar"~total_count/(162+30+36),
                        date=="21-Feb"~total_count/(131+16+28),
                        date=="21-Jan"~total_count/(177+21+31),
                        date=="21-Mar"~total_count/(125+11+21),
                        date=="22-Jan"~total_count/(195+15+34),
                        date=="23-Jan"~total_count/(186+20+35),
                        date=="24-Feb"~total_count/(174+15+35),
                        date=="24-Jan"~total_count/(130+12+25),
                        date=="24-Mar"~total_count/(135+15+51),
                        date=="25-Feb"~total_count/(203+28+34),
                        date=="25-Mar"~total_count/(208+24+29),
                        date=="26-Feb"~total_count/(153+13+46),
                        date=="26-Mar"~total_count/(160+23+30),
                        date=="27-Feb"~total_count/(190+20+38),
                        date=="27-Jan"~total_count/(172+15+44),
                        date=="27-Mar"~total_count/(160+21+27),
                        date=="28-Feb"~total_count/(121+12+25),
                        date=="28-Jan"~total_count/(193+26+36),
                        date=="28-Mar"~total_count/(62+11+16),
                        date=="29-Jan"~total_count/(173+25+28),
                        date=="3-Feb"~total_count/(171+12+35),
                        date=="3-Mar"~total_count/(167+19+37),
                        date=="30-Jan"~total_count/(192+29+30),
                        date=="31-Jan"~total_count/(136+14+20),
                        date=="4-Feb"~total_count/(194+28+28),
                        date=="4-Mar"~total_count/(179+22+39),
                        date=="5-Feb"~total_count/(175+17+36),
                        date=="5-Mar"~total_count/(158+23+26),
                        date=="6-Feb"~total_count/(98+12),
                        date=="6-Mar"~total_count/(187+25+23),
                        date=="7-Apr"~total_count/(128+21+24),
                        date=="7-Feb"~total_count/(129+20+21),
                        date=="7-Mar"~total_count/(132+17+17),
                        date=="8-Apr"~total_count/(197+23+52),
                        date=="9-Apr"~total_count/(158+15+46))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Lowest-Carbon Offering") %>% ### Lowest-carbon offering
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.08288481,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.08781895,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.08407254,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.09260956,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.09581833,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.08788481) +
  annotate("point",x=as.Date("2025-02-14"),y=0.09281895) +
  annotate("point",x=as.Date("2025-03-03"),y=0.08907254) +
  annotate("point",x=as.Date("2025-03-17"),y=0.09760956) +
  annotate("point",x=as.Date("2025-04-06"),y=0.10081833) +
  labs(title="Ramen & Grill Stations (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_prop_low_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-127-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"High",
                        item=="Grilled Hamburger"~"High",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(date,phase_interval,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  arrange(date)
```

    ## `summarise()` has grouped output by 'date', 'phase_interval', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 170 × 4
    ## # Groups:   date, phase_interval [57]
    ##    date      phase_interval item                   total_count
    ##    <chr>     <chr>          <chr>                        <int>
    ##  1 2025-1-21 1              High                           177
    ##  2 2025-1-21 1              Lowest-Carbon Offering          21
    ##  3 2025-1-21 1              Middle                          31
    ##  4 2025-1-22 1              High                           195
    ##  5 2025-1-22 1              Lowest-Carbon Offering          15
    ##  6 2025-1-22 1              Middle                          34
    ##  7 2025-1-23 1              High                           186
    ##  8 2025-1-23 1              Lowest-Carbon Offering          20
    ##  9 2025-1-23 1              Middle                          35
    ## 10 2025-1-24 1              High                           130
    ## # ℹ 160 more rows

FOR CONTROL PASTA STATION

``` r
spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(1126+226),
                        phase_interval=="2"~total_count/(1336+296),
                        phase_interval=="3"~total_count/(1017+207),
                        phase_interval=="4"~total_count/(945+213),
                        phase_interval=="5"~total_count/(1319+336))) %>%
    filter(item=="Create Your Pasta Bowl VEG")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item                       total_count  prop
    ##   <chr>          <chr>   <chr>                            <int> <dbl>
    ## 1 1              Pasta   Create Your Pasta Bowl VEG         226 0.167
    ## 2 2              Pasta   Create Your Pasta Bowl VEG         296 0.181
    ## 3 3              Pasta   Create Your Pasta Bowl VEG         207 0.169
    ## 4 4              Pasta   Create Your Pasta Bowl VEG         213 0.184
    ## 5 5              Pasta   Create Your Pasta Bowl VEG         336 0.203

``` r
spring_prop_low_control <- spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(96+29),
                        date=="10-Feb"~total_count/(121+19),
                        date=="10-Mar"~total_count/(106+20),
                        date=="11-Apr"~total_count/(56+10),
                        date=="11-Feb"~total_count/(111+23),
                        date=="11-Mar"~total_count/(81+29),
                        date=="12-Feb"~total_count/(133+22),
                        date=="12-Mar"~total_count/(107+20),
                        date=="13-Feb"~total_count/(81+20),
                        date=="13-Mar"~total_count/(92+19),
                        date=="14-Apr"~total_count/(100+28),
                        date=="14-Feb"~total_count/(34+16),
                        date=="14-Mar"~total_count/(71+14),
                        date=="15-Apr"~total_count/(83+23),
                        date=="16-Apr"~total_count/(118+26),
                        date=="17-Apr"~total_count/(85+27),
                        date=="17-Mar"~total_count/(126+18),
                        date=="18-Apr"~total_count/(63+18),
                        date=="18-Mar"~total_count/(89+27),
                        date=="19-Feb"~total_count/(104+28),
                        date=="19-Mar"~total_count/(106+20),
                        date=="20-Feb"~total_count/(93+27),
                        date=="20-Mar"~total_count/(98+28),
                        date=="21-Feb"~total_count/(78+16),
                        date=="21-Jan"~total_count/(138+42),
                        date=="21-Mar"~total_count/(69+18),
                        date=="22-Jan"~total_count/(175+23),
                        date=="23-Jan"~total_count/(105+27),
                        date=="24-Feb"~total_count/(130+22),
                        date=="24-Jan"~total_count/(86+15),
                        date=="24-Mar"~total_count/(87+21),
                        date=="25-Feb"~total_count/(105+16),
                        date=="25-Mar"~total_count/(94+24),
                        date=="26-Feb"~total_count/(136+19),
                        date=="26-Mar"~total_count/(103+20),
                        date=="27-Feb"~total_count/(89+30),
                        date=="27-Jan"~total_count/(162+21),
                        date=="27-Mar"~total_count/(74+27),
                        date=="28-Feb"~total_count/(84+13),
                        date=="28-Jan"~total_count/(102+35),
                        date=="28-Mar"~total_count/(34+10),
                        date=="29-Jan"~total_count/(150+22),
                        date=="3-Feb"~total_count/(140+18),
                        date=="3-Mar"~total_count/(107+13),
                        date=="30-Jan"~total_count/(118+33),
                        date=="31-Jan"~total_count/(90+8),
                        date=="4-Feb"~total_count/(101+24),
                        date=="4-Mar"~total_count/(86+29),
                        date=="5-Feb"~total_count/(144+26),
                        date=="5-Mar"~total_count/(132+20),
                        date=="6-Feb"~total_count/(116+42),
                        date=="6-Mar"~total_count/(93+23),
                        date=="7-Apr"~total_count/(93+21),
                        date=="7-Feb"~total_count/(80+15),
                        date=="7-Mar"~total_count/(55+22),
                        date=="8-Apr"~total_count/(108+29),
                        date=="9-Apr"~total_count/(125+23))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Create Your Pasta Bowl VEG") %>% ### Lowest-carbon offering
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Create Your Pasta Bowl VEG"~"Lowest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.1471598,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.1613725,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.1491176,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.1639378,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.1830211,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.1671598) +
  annotate("point",x=as.Date("2025-02-14"),y=0.1813725) +
  annotate("point",x=as.Date("2025-03-03"),y=0.1691176) +
  annotate("point",x=as.Date("2025-03-17"),y=0.1839378) +
  annotate("point",x=as.Date("2025-04-06"),y=0.2030211) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
spring_prop_low_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-130-1.png)<!-- -->

``` r
spring_prop_low <- ggarrange(spring_prop_low_ramen,spring_prop_low_grill,spring_prop_low_treatment,spring_prop_low_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
spring_prop_low <- annotate_figure(spring_prop_low,top=text_grob("Daily Sales Percentages of Lowest-Carbon Offerings (%)", 
               color="black",face="bold",size=12))
ggsave(filename="spring_prop_low.png",plot=spring_prop_low,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
spring_prop_low
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-131-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(25+135+814+72+76),
                        phase_interval=="2"~total_count/(30+193+1083+97+74),
                        phase_interval=="3"~total_count/(18+167+938+87+66),
                        phase_interval=="4"~total_count/(33+160+909+67+74),
                        phase_interval=="5"~total_count/(45+250+1352+130+125))) %>%
  filter(item=="Grilled Hamburger")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item              total_count  prop
    ##   <chr>          <chr>   <chr>                   <int> <dbl>
    ## 1 1              Grill   Grilled Hamburger         814 0.725
    ## 2 2              Grill   Grilled Hamburger        1083 0.733
    ## 3 3              Grill   Grilled Hamburger         938 0.735
    ## 4 4              Grill   Grilled Hamburger         909 0.731
    ## 5 5              Grill   Grilled Hamburger        1352 0.711

``` r
spring_prop_high_grill <- daily_prop_spring_data %>%
  filter(item=="Grilled Hamburger") %>%
  mutate(item=case_when(item=="Grilled Hamburger"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.7154902,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.7232431,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.7251097,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.7212953,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.7008307,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.7254902) +
  annotate("point",x=as.Date("2025-02-14"),y=0.7332431) +
  annotate("point",x=as.Date("2025-03-03"),y=0.7351097) +
  annotate("point",x=as.Date("2025-03-17"),y=0.7312953) +
  annotate("point",x=as.Date("2025-04-06"),y=0.7108307) +
  labs(title="Grill Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
spring_prop_high_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-133-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(740+152),
                        phase_interval=="2"~total_count/(928+213),
                        phase_interval=="3"~total_count/(726+176),
                        phase_interval=="4"~total_count/(602+163),
                        phase_interval=="5"~total_count/(890+262))) %>%
  filter(item=="Bowl Ramen Chicken")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item               total_count  prop
    ##   <chr>          <chr>   <chr>                    <int> <dbl>
    ## 1 1              Ramen   Bowl Ramen Chicken         740 0.830
    ## 2 2              Ramen   Bowl Ramen Chicken         928 0.813
    ## 3 3              Ramen   Bowl Ramen Chicken         726 0.805
    ## 4 4              Ramen   Bowl Ramen Chicken         602 0.787
    ## 5 5              Ramen   Bowl Ramen Chicken         890 0.773

``` r
spring_prop_high_ramen <- spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(80+24),
                        date=="10-Feb"~total_count/(58+21),
                        date=="10-Mar"~total_count/(38+6),
                        date=="11-Apr"~total_count/(46+8),
                        date=="11-Feb"~total_count/(95+20),
                        date=="11-Mar"~total_count/(51+11),
                        date=="12-Feb"~total_count/(79+11),
                        date=="12-Mar"~total_count/(72+17),
                        date=="13-Feb"~total_count/(66+21),
                        date=="13-Mar"~total_count/(75+24),
                        date=="14-Apr"~total_count/(38+27),
                        date=="14-Feb"~total_count/(43+15),
                        date=="14-Mar"~total_count/(47+11),
                        date=="15-Apr"~total_count/(71+28),
                        date=="16-Apr"~total_count/(61+13),
                        date=="17-Apr"~total_count/(58+24),
                        date=="17-Mar"~total_count/(84+19),
                        date=="18-Apr"~total_count/(46+12),
                        date=="18-Mar"~total_count/(74+23),
                        date=="19-Feb"~total_count/(81+15),
                        date=="19-Mar"~total_count/(62+15),
                        date=="20-Feb"~total_count/(62+15),
                        date=="20-Mar"~total_count/(59+27),
                        date=="21-Feb"~total_count/(58+14),
                        date=="21-Jan"~total_count/(83+19),
                        date=="21-Mar"~total_count/(40+10),
                        date=="22-Jan"~total_count/(90+12),
                        date=="23-Jan"~total_count/(92+18),
                        date=="24-Feb"~total_count/(79+14),
                        date=="24-Jan"~total_count/(67+11),
                        date=="24-Mar"~total_count/(54+13),
                        date=="25-Feb"~total_count/(101+24),
                        date=="25-Mar"~total_count/(86+19),
                        date=="26-Feb"~total_count/(63+12),
                        date=="26-Mar"~total_count/(52+16),
                        date=="27-Feb"~total_count/(85+18),
                        date=="27-Jan"~total_count/(80+11),
                        date=="27-Mar"~total_count/(61+20),
                        date=="28-Feb"~total_count/(49+11),
                        date=="28-Jan"~total_count/(88+21),
                        date=="28-Mar"~total_count/(33+6),
                        date=="29-Jan"~total_count/(85+21),
                        date=="3-Feb"~total_count/(81+12),
                        date=="3-Mar"~total_count/(72+17),
                        date=="30-Jan"~total_count/(83+26),
                        date=="31-Jan"~total_count/(72+13),
                        date=="4-Feb"~total_count/(86+25),
                        date=="4-Mar"~total_count/(75+20),
                        date=="5-Feb"~total_count/(78+14),
                        date=="5-Mar"~total_count/(72+20),
                        date=="6-Feb"~total_count/(98+12),
                        date=="6-Mar"~total_count/(73+24),
                        date=="7-Apr"~total_count/(51+17),
                        date=="7-Feb"~total_count/(43+18),
                        date=="7-Mar"~total_count/(57+16),
                        date=="8-Apr"~total_count/(84+21),
                        date=="9-Apr"~total_count/(69+14))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Bowl Ramen Chicken") %>% ### Highest-carbon offering
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Bowl Ramen Chicken"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.8095964,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.7933216,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.7848780,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.7669281,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.7525694,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.8295964) +
  annotate("point",x=as.Date("2025-02-14"),y=0.8133216) +
  annotate("point",x=as.Date("2025-03-03"),y=0.8048780) +
  annotate("point",x=as.Date("2025-03-17"),y=0.7869281) +
  annotate("point",x=as.Date("2025-04-06"),y=0.7725694) +
  labs(title="Ramen Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval', 'station'. You
    ## can override using the `.groups` argument.

``` r
spring_prop_high_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-135-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count))  %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(25+135+814+72+76+740+152),
                        phase_interval=="2"~total_count/(30+193+1083+97+74+928+213),
                        phase_interval=="3"~total_count/(18+167+938+87+66+726+176),
                        phase_interval=="4"~total_count/(33+160+909+67+74+602+163),
                        phase_interval=="5"~total_count/(46+250+1352+130+125+890+262))) %>%
   mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(phase_interval,item) %>%
  summarise(prop=sum(prop)) %>%
  filter(item=="Highest-Carbon Offering")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   phase_interval [5]
    ##   phase_interval item                     prop
    ##   <chr>          <chr>                   <dbl>
    ## 1 1              Highest-Carbon Offering 0.772
    ## 2 2              Highest-Carbon Offering 0.768
    ## 3 3              Highest-Carbon Offering 0.764
    ## 4 4              Highest-Carbon Offering 0.752
    ## 5 5              Highest-Carbon Offering 0.734

``` r
spring_prop_high_treatment <- spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest-Carbon Offering",
                        item=="Bowl Ramen Chicken"~"Highest-Carbon Offering",
                        item=="Grilled Hamburger"~"Highest-Carbon Offering",
                        item=="Grilled Chicken Breast Sandwich"~"Middle",
                        item=="Seared Salmon Burger"~"Middle",
                        item=="Trillium Grill Impossible Burger"~"Middle")) %>%
  ungroup() %>%
  group_by(date,phase_interval,item) %>%
  summarise(total_count=sum(total_count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(170+30+35),
                        date=="10-Feb"~total_count/(159+25+32),
                        date=="10-Mar"~total_count/(124+9+28),
                        date=="11-Apr"~total_count/(118+9+30),
                        date=="11-Feb"~total_count/(181+23+40),
                        date=="11-Mar"~total_count/(169+14+31),
                        date=="12-Feb"~total_count/(180+14+40),
                        date=="12-Mar"~total_count/(161+23+33),
                        date=="13-Feb"~total_count/(149+24+33),
                        date=="13-Mar"~total_count/(174+24+30),
                        date=="14-Apr"~total_count/(117+30+43),
                        date=="14-Feb"~total_count/(115+17+14),
                        date=="14-Mar"~total_count/(123+12+25),
                        date=="15-Apr"~total_count/(172+30+38),
                        date=="16-Apr"~total_count/(164+16+31),
                        date=="17-Apr"~total_count/(171+26+34),
                        date=="17-Mar"~total_count/(161+27+39),
                        date=="18-Apr"~total_count/(122+14+19),
                        date=="18-Mar"~total_count/(154+26+29),
                        date=="19-Feb"~total_count/(165+16+25),
                        date=="19-Mar"~total_count/(158+20+29),
                        date=="20-Feb"~total_count/(164+19+32),
                        date=="20-Mar"~total_count/(162+30+36),
                        date=="21-Feb"~total_count/(131+16+28),
                        date=="21-Jan"~total_count/(177+21+31),
                        date=="21-Mar"~total_count/(125+11+21),
                        date=="22-Jan"~total_count/(195+15+34),
                        date=="23-Jan"~total_count/(186+20+35),
                        date=="24-Feb"~total_count/(174+15+35),
                        date=="24-Jan"~total_count/(130+12+25),
                        date=="24-Mar"~total_count/(135+15+51),
                        date=="25-Feb"~total_count/(203+28+34),
                        date=="25-Mar"~total_count/(208+24+29),
                        date=="26-Feb"~total_count/(153+13+46),
                        date=="26-Mar"~total_count/(160+23+30),
                        date=="27-Feb"~total_count/(190+20+38),
                        date=="27-Jan"~total_count/(172+15+44),
                        date=="27-Mar"~total_count/(160+21+27),
                        date=="28-Feb"~total_count/(121+12+25),
                        date=="28-Jan"~total_count/(193+26+36),
                        date=="28-Mar"~total_count/(62+11+16),
                        date=="29-Jan"~total_count/(173+25+28),
                        date=="3-Feb"~total_count/(171+12+35),
                        date=="3-Mar"~total_count/(167+19+37),
                        date=="30-Jan"~total_count/(192+29+30),
                        date=="31-Jan"~total_count/(136+14+20),
                        date=="4-Feb"~total_count/(194+28+28),
                        date=="4-Mar"~total_count/(179+22+39),
                        date=="5-Feb"~total_count/(175+17+36),
                        date=="5-Mar"~total_count/(158+23+26),
                        date=="6-Feb"~total_count/(98+12),
                        date=="6-Mar"~total_count/(187+25+23),
                        date=="7-Apr"~total_count/(128+21+24),
                        date=="7-Feb"~total_count/(129+20+21),
                        date=="7-Mar"~total_count/(132+17+17),
                        date=="8-Apr"~total_count/(197+23+52),
                        date=="9-Apr"~total_count/(158+15+46))) %>%
   mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Highest-Carbon Offering") %>% ### Highest-carbon offering
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.7515988,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.7481436,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.7440037,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.7324900,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.7138789,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.7715988) +
  annotate("point",x=as.Date("2025-02-14"),y=0.7681436) +
  annotate("point",x=as.Date("2025-03-03"),y=0.7640037) +
  annotate("point",x=as.Date("2025-03-17"),y=0.7524900) +
  annotate("point",x=as.Date("2025-04-06"),y=0.7338789) +
  labs(title="Ramen & Grill Stations (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_prop_high_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-137-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(phase_interval=="1"~total_count/(1126+226),
                        phase_interval=="2"~total_count/(1336+296),
                        phase_interval=="3"~total_count/(1017+207),
                        phase_interval=="4"~total_count/(945+213),
                        phase_interval=="5"~total_count/(1319+336))) %>%
    filter(item=="Create Your Pasta Bowl MEAT")
```

    ## `summarise()` has grouped output by 'phase_interval', 'station'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval, station [5]
    ##   phase_interval station item                        total_count  prop
    ##   <chr>          <chr>   <chr>                             <int> <dbl>
    ## 1 1              Pasta   Create Your Pasta Bowl MEAT        1126 0.833
    ## 2 2              Pasta   Create Your Pasta Bowl MEAT        1336 0.819
    ## 3 3              Pasta   Create Your Pasta Bowl MEAT        1017 0.831
    ## 4 4              Pasta   Create Your Pasta Bowl MEAT         945 0.816
    ## 5 5              Pasta   Create Your Pasta Bowl MEAT        1319 0.797

``` r
spring_prop_high_control <- spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(prop=case_when(date=="10-Apr"~total_count/(96+29),
                        date=="10-Feb"~total_count/(121+19),
                        date=="10-Mar"~total_count/(106+20),
                        date=="11-Apr"~total_count/(56+10),
                        date=="11-Feb"~total_count/(111+23),
                        date=="11-Mar"~total_count/(81+29),
                        date=="12-Feb"~total_count/(133+22),
                        date=="12-Mar"~total_count/(107+20),
                        date=="13-Feb"~total_count/(81+20),
                        date=="13-Mar"~total_count/(92+19),
                        date=="14-Apr"~total_count/(100+28),
                        date=="14-Feb"~total_count/(34+16),
                        date=="14-Mar"~total_count/(71+14),
                        date=="15-Apr"~total_count/(83+23),
                        date=="16-Apr"~total_count/(118+26),
                        date=="17-Apr"~total_count/(85+27),
                        date=="17-Mar"~total_count/(126+18),
                        date=="18-Apr"~total_count/(63+18),
                        date=="18-Mar"~total_count/(89+27),
                        date=="19-Feb"~total_count/(104+28),
                        date=="19-Mar"~total_count/(106+20),
                        date=="20-Feb"~total_count/(93+27),
                        date=="20-Mar"~total_count/(98+28),
                        date=="21-Feb"~total_count/(78+16),
                        date=="21-Jan"~total_count/(138+42),
                        date=="21-Mar"~total_count/(69+18),
                        date=="22-Jan"~total_count/(175+23),
                        date=="23-Jan"~total_count/(105+27),
                        date=="24-Feb"~total_count/(130+22),
                        date=="24-Jan"~total_count/(86+15),
                        date=="24-Mar"~total_count/(87+21),
                        date=="25-Feb"~total_count/(105+16),
                        date=="25-Mar"~total_count/(94+24),
                        date=="26-Feb"~total_count/(136+19),
                        date=="26-Mar"~total_count/(103+20),
                        date=="27-Feb"~total_count/(89+30),
                        date=="27-Jan"~total_count/(162+21),
                        date=="27-Mar"~total_count/(74+27),
                        date=="28-Feb"~total_count/(84+13),
                        date=="28-Jan"~total_count/(102+35),
                        date=="28-Mar"~total_count/(34+10),
                        date=="29-Jan"~total_count/(150+22),
                        date=="3-Feb"~total_count/(140+18),
                        date=="3-Mar"~total_count/(107+13),
                        date=="30-Jan"~total_count/(118+33),
                        date=="31-Jan"~total_count/(90+8),
                        date=="4-Feb"~total_count/(101+24),
                        date=="4-Mar"~total_count/(86+29),
                        date=="5-Feb"~total_count/(144+26),
                        date=="5-Mar"~total_count/(132+20),
                        date=="6-Feb"~total_count/(116+42),
                        date=="6-Mar"~total_count/(93+23),
                        date=="7-Apr"~total_count/(93+21),
                        date=="7-Feb"~total_count/(80+15),
                        date=="7-Mar"~total_count/(55+22),
                        date=="8-Apr"~total_count/(108+29),
                        date=="9-Apr"~total_count/(125+23))) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  filter(item=="Create Your Pasta Bowl MEAT") %>% ### Highest-carbon offering
  mutate(date=as.Date(date)) %>%
  mutate(item=case_when(item=="Create Your Pasta Bowl MEAT"~"Highest-Carbon Offering")) %>%
  ggplot(aes(x=date,y=prop,color=item,fill=item)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.8128402,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.7986275,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.8108824,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.7960622,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.7769789,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.8328402) +
  annotate("point",x=as.Date("2025-02-14"),y=0.8186275) +
  annotate("point",x=as.Date("2025-03-03"),y=0.8308824) +
  annotate("point",x=as.Date("2025-03-17"),y=0.8160622) +
  annotate("point",x=as.Date("2025-04-06"),y=0.7969789) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
spring_prop_high_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-139-1.png)<!-- -->

``` r
spring_prop_high <- ggarrange(spring_prop_high_ramen,spring_prop_high_grill,spring_prop_high_treatment,spring_prop_high_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
spring_prop_high <- annotate_figure(spring_prop_high,top=text_grob("Daily Sales Percentages of Highest-Carbon Offerings (%)", 
               color="black",face="bold",size = 12))
ggsave(filename="spring_prop_high.png",plot=spring_prop_high,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
spring_prop_high
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-140-1.png)<!-- -->

mean_carbon

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval [5]
    ##   phase_interval station total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>          <chr>               <dbl>       <int>            <dbl>
    ## 1 1              Grill               3251.        1122             2.90
    ## 2 2              Grill               4324.        1477             2.93
    ## 3 3              Grill               3746.        1276             2.94
    ## 4 4              Grill               3626.        1243             2.92
    ## 5 5              Grill               5412.        1903             2.84

``` r
spring_mean_carbon_grill <- spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=2.897567-0.04,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=2.927513-0.04,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=2.935598-0.04,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=2.916774-0.04,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=2.844186-0.04,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=2.897567) +
  annotate("point",x=as.Date("2025-02-14"),y=2.927513) +
  annotate("point",x=as.Date("2025-03-03"),y=2.935598) +
  annotate("point",x=as.Date("2025-03-17"),y=2.916774) +
  annotate("point",x=as.Date("2025-04-06"),y=2.844186) +
  labs(title="Grill Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_carbon_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-142-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## `summarise()` has grouped output by 'phase_interval'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   phase_interval [5]
    ##   phase_interval station total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>          <chr>               <dbl>       <int>            <dbl>
    ## 1 1              Ramen                328.         892            0.367
    ## 2 2              Ramen                418.        1141            0.366
    ## 3 3              Ramen                330.         902            0.365
    ## 4 4              Ramen                279.         765            0.364
    ## 5 5              Ramen                418.        1152            0.363

``` r
spring_mean_carbon_ramen <- spring_data %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.3672164-0.001,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.3660254-0.001,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.3654076-0.001,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.3640941-0.001,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.3630433-0.001,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.3672164) +
  annotate("point",x=as.Date("2025-02-14"),y=0.3660254) +
  annotate("point",x=as.Date("2025-03-03"),y=0.3654076) +
  annotate("point",x=as.Date("2025-03-17"),y=0.3640941) +
  annotate("point",x=as.Date("2025-04-06"),y=0.3630433) +
  labs(title="Ramen Station (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_carbon_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-144-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                          3579.        2014             1.78
    ## 2 2                          4742.        2618             1.81
    ## 3 3                          4075.        2178             1.87
    ## 4 4                          3904.        2008             1.94
    ## 5 5                          5831.        3055             1.91

``` r
spring_mean_carbon_treatment <- spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station_type) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station_type,fill=station_type)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=1.776875-0.1,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=1.811143-0.1,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=1.871176-0.1,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=1.944264-0.1,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=1.908580-0.1,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=1.776875) +
  annotate("point",x=as.Date("2025-02-14"),y=1.811143) +
  annotate("point",x=as.Date("2025-03-03"),y=1.871176) +
  annotate("point",x=as.Date("2025-03-17"),y=1.944264) +
  annotate("point",x=as.Date("2025-04-06"),y=1.908580) +
  labs(title="Ramen & Grill Stations (Treated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_carbon_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-146-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>% 
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales)
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_carbon_cost total_sales mean_carbon_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                           736.        1352            0.544
    ## 2 2                           877.        1632            0.538
    ## 3 3                           665.        1224            0.544
    ## 4 4                           621.        1158            0.536
    ## 5 5                           872.        1655            0.527

``` r
spring_mean_carbon_control <- spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station) %>%
  summarise(total_carbon_cost=sum(corr_carbon_cost),total_sales=sum(count)) %>%
  mutate(mean_carbon_cost=total_carbon_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_carbon_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab(bquote('Kilograms of CO'[2]*'e')) +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=0.5444727-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.5375752-0.01,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.5435225-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.5363303-0.01,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.5270691-0.01,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=0.5444727) +
  annotate("point",x=as.Date("2025-02-14"),y=0.5375752) +
  annotate("point",x=as.Date("2025-03-03"),y=0.5435225) +
  annotate("point",x=as.Date("2025-03-17"),y=0.5363303) +
  annotate("point",x=as.Date("2025-04-06"),y=0.5270691) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_carbon_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-148-1.png)<!-- -->

``` r
spring_mean_carbon <- ggarrange(spring_mean_carbon_ramen,spring_mean_carbon_grill,spring_mean_carbon_treatment,spring_mean_carbon_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
spring_mean_carbon <- annotate_figure(spring_mean_carbon,top=text_grob("Mean Emissions Cost of Station Sales (kg CO2e)", 
               color="black",face="bold",size=12))
ggsave(filename="spring_mean_carbon.png",plot=spring_mean_carbon,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
spring_mean_carbon
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-149-1.png)<!-- -->
mean_spend

``` r
spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                         10090.        1122             8.99
    ## 2 2                         13235.        1477             8.96
    ## 3 3                         11438.        1276             8.96
    ## 4 4                         11162.        1243             8.98
    ## 5 5                         17092.        1903             8.98

``` r
spring_mean_spend_grill <- spring_data %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  labs(title="Grill Station (Treated)") +
  annotate("text",x=as.Date("2025-01-27"),y=8.992674-0.015,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=8.960887-0.015,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=8.963824-0.015,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=8.980024-0.015,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=8.981435-0.015,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=8.992674) +
  annotate("point",x=as.Date("2025-02-14"),y=8.960887) +
  annotate("point",x=as.Date("2025-03-03"),y=8.963824) +
  annotate("point",x=as.Date("2025-03-17"),y=8.980024) +
  annotate("point",x=as.Date("2025-04-06"),y=8.981435) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_spend_grill
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-151-1.png)<!-- -->

``` r
spring_data %>% 
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                          8465.         892             9.49
    ## 2 2                         10828.        1141             9.49
    ## 3 3                          8560.         902             9.49
    ## 4 4                          7260.         765             9.49
    ## 5 5                         10932.        1152             9.49

``` r
spring_mean_spend_ramen <- spring_data %>% 
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  labs(title="Ramen Station (Treated)") +
  annotate("text",x=as.Date("2025-01-27"),y=9.49-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=9.49-0.01,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=9.49-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=9.49-0.01,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=9.49-0.01,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=9.49) +
  annotate("point",x=as.Date("2025-02-14"),y=9.49) +
  annotate("point",x=as.Date("2025-03-03"),y=9.49) +
  annotate("point",x=as.Date("2025-03-17"),y=9.49) +
  annotate("point",x=as.Date("2025-04-06"),y=9.49) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_spend_ramen
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-153-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales)
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                         18555.        2014             9.21
    ## 2 2                         24063.        2618             9.19
    ## 3 3                         19998.        2178             9.18
    ## 4 4                         18422.        2008             9.17
    ## 5 5                         28024.        3055             9.17

``` r
spring_mean_spend_treatment <- spring_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,phase_interval,station_type) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station_type,fill=station_type)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Dark2",name="") +
  scale_fill_brewer(palette="Dark2",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  labs(title="Ramen & Grill Stations (Treated)") +
  annotate("text",x=as.Date("2025-01-27"),y=9.212939-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=9.191490-0.01,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=9.181736-0.01,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=9.174313-0.01,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=9.173208-0.01,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=9.212939) +
  annotate("point",x=as.Date("2025-02-14"),y=9.191490) +
  annotate("point",x=as.Date("2025-03-03"),y=9.181736) +
  annotate("point",x=as.Date("2025-03-17"),y=9.174313) +
  annotate("point",x=as.Date("2025-04-06"),y=9.173208) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'phase_interval'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_spend_treatment
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-155-1.png)<!-- -->

``` r
spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(phase_interval) %>% 
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) 
```

    ## # A tibble: 5 × 4
    ##   phase_interval total_dollar_cost total_sales mean_dollar_cost
    ##   <chr>                      <dbl>       <int>            <dbl>
    ## 1 1                         12041.        1352             8.91
    ## 2 2                         14524.        1632             8.90
    ## 3 3                         10900.        1224             8.91
    ## 4 4                         10304.        1158             8.90
    ## 5 5                         14710.        1655             8.89

``` r
spring_mean_spend_control <- spring_data %>%
  filter(station=="Pasta") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station) %>%
  summarise(total_dollar_cost=sum(corr_dollar_cost),total_sales=sum(count)) %>%
  mutate(mean_dollar_cost=total_dollar_cost/total_sales) %>%
  mutate(date=case_when(date=="10-Apr"~"2025-4-10",
                        date=="10-Feb"~"2025-2-10",
                        date=="10-Mar"~"2025-3-10",
                        date=="11-Apr"~"2025-4-11",
                        date=="11-Feb"~"2025-2-11",
                        date=="11-Mar"~"2025-3-11",
                        date=="12-Feb"~"2025-2-12",
                        date=="12-Mar"~"2025-3-12",
                        date=="13-Feb"~"2025-2-13",
                        date=="13-Mar"~"2025-3-13",
                        date=="14-Apr"~"2025-4-14",
                        date=="14-Feb"~"2025-2-14",
                        date=="14-Mar"~"2025-3-14",
                        date=="15-Apr"~"2025-4-15",
                        date=="16-Apr"~"2025-4-16",
                        date=="17-Apr"~"2025-4-17",
                        date=="17-Mar"~"2025-3-17",
                        date=="18-Apr"~"2025-4-18",
                        date=="18-Mar"~"2025-3-18",
                        date=="19-Feb"~"2025-2-19",
                        date=="19-Mar"~"2025-3-19",
                        date=="20-Feb"~"2025-2-20",
                        date=="20-Mar"~"2025-3-20",
                        date=="21-Feb"~"2025-2-21",
                        date=="21-Jan"~"2025-1-21",
                        date=="21-Mar"~"2025-3-21",
                        date=="22-Jan"~"2025-1-22",
                        date=="23-Jan"~"2025-1-23",
                        date=="24-Feb"~"2025-2-24",
                        date=="24-Jan"~"2025-1-24",
                        date=="24-Mar"~"2025-3-24",
                        date=="25-Feb"~"2025-2-25",
                        date=="25-Mar"~"2025-3-25",
                        date=="26-Feb"~"2025-2-26",
                        date=="26-Mar"~"2025-3-26",
                        date=="27-Feb"~"2025-2-27",
                        date=="27-Jan"~"2025-1-27",
                        date=="27-Mar"~"2025-3-27",
                        date=="28-Feb"~"2025-2-28",
                        date=="28-Jan"~"2025-1-28",
                        date=="28-Mar"~"2025-3-28",
                        date=="29-Jan"~"2025-1-29",
                        date=="3-Feb"~"2025-2-3",
                        date=="3-Mar"~"2025-3-3",
                        date=="30-Jan"~"2025-1-30",
                        date=="31-Jan"~"2025-1-31",
                        date=="4-Feb"~"2025-2-4",
                        date=="4-Mar"~"2025-3-4",
                        date=="5-Feb"~"2025-2-5",
                        date=="5-Mar"~"2025-3-5",
                        date=="6-Feb"~"2025-2-6",
                        date=="6-Mar"~"2025-3-6",
                        date=="7-Apr"~"2025-4-7",
                        date=="7-Feb"~"2025-2-7",
                        date=="7-Mar"~"2025-3-7",
                        date=="8-Apr"~"2025-4-8",
                        date=="9-Apr"~"2025-4-9")) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=mean_dollar_cost,color=station,fill=station)) + 
  geom_smooth(alpha=0.2) + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  xlab("Date") + 
  ylab("U.S. Dollars ($)") +
  scale_color_brewer(palette="Set1",name="") +
  scale_fill_brewer(palette="Set1",name="") +
  scale_x_date(breaks=as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2025-04-18")),labels=c("Jan 21","Feb 3","Feb 24","Mar 10","Mar 24","Apr 18")) +
  annotate("text",x=as.Date("2025-01-27"),y=8.906420-0.005,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=8.899314-0.005,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=8.905441-0.005,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=8.898031-0.005,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=8.888489-0.005,label="Control",size=10/.pt) +
  annotate("point",x=as.Date("2025-01-27"),y=8.906420) +
  annotate("point",x=as.Date("2025-02-14"),y=8.899314) +
  annotate("point",x=as.Date("2025-03-03"),y=8.905441) +
  annotate("point",x=as.Date("2025-03-17"),y=8.898031) +
  annotate("point",x=as.Date("2025-04-06"),y=8.888489) +
  labs(title="Pasta Station (Untreated)") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.title=element_text(size=10),axis.text=element_text(size=10)) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
spring_mean_spend_control
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-157-1.png)<!-- -->

``` r
spring_mean_spend <- ggarrange(spring_mean_spend_ramen,spring_mean_spend_grill,spring_mean_spend_treatment,spring_mean_spend_control,
          labels=c("A","B","C","D"),
          legend="none")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
spring_mean_spend <- annotate_figure(spring_mean_spend,top=text_grob("Mean Revenue Per Station Sale ($)",color="black",face="bold",size=12))
ggsave(filename="spring_mean_spend.png",plot=spring_mean_spend,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=40,height=24,units="cm",dpi=150,limitsize=TRUE)
spring_mean_spend
```

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-158-1.png)<!-- -->

REVISE FIGURE NAMING NOMENCLATURE from semester-station-outcome TO
SEMESTER-OUTCOME-STATION

We’ll need to do checks to see whether there are differences in the
purchase of different item categories across menu conditions, whether
there are differences in the sales total across menu conditions,
differences in the number of observation days across menu conditions,
differences in the daily sales average across menu conditions

Spillover within meal period and between meal period

also look at diminishing effects over time by looking at outcomes at day
level, not just by period

PROP MIDDLE STUFF

``` r
grill_prop_middle_s2 <- daily_prop_spring_data %>%
  filter(item=="Grilled Chicken Breast Sandwich"|item=="Seared Salmon Burger"|item=="Trillium Grill Impossible Burger") %>%
  ggplot(aes(x=date,y=prop,color=item)) + 
  geom_smooth() +
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  scale_color_brewer(palette="Set2") + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) + 
  annotate("text",x=as.Date("2025-01-27"),y=0.175,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.175,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.175,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.175,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.175,label="Control",size=10/.pt) 
grill_prop_middle_s2
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-159-1.png)<!-- -->

``` r
daily_prop_spring_data %>%
  ggplot(aes(x=date,y=prop,color=item)) + 
  geom_smooth() +
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  scale_color_brewer(palette="Set2") + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) + 
  annotate("text",x=as.Date("2025-01-27"),y=0.5,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.5,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.5,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.5,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.5,label="Control",size=10/.pt) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-160-1.png)<!-- -->

## Preliminary Checks

need to figure out modifications sales between periods because that
changes the denominator

``` r
fall_data %>%
  filter(station=="Grill") %>%
  group_by(menu_condition,item_cat) %>%
  summarise(sum(count)) 
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 12 × 3
    ## # Groups:   menu_condition [4]
    ##    menu_condition item_cat     `sum(count)`
    ##    <chr>          <chr>               <int>
    ##  1 Carbon Label   Main                 1285
    ##  2 Carbon Label   Modification          259
    ##  3 Carbon Label   Side                 1506
    ##  4 Control        Main                 1048
    ##  5 Control        Modification          203
    ##  6 Control        Side                 1143
    ##  7 Default        Main                 1270
    ##  8 Default        Modification          271
    ##  9 Default        Side                 1431
    ## 10 Multimodal     Main                 1595
    ## 11 Multimodal     Modification          353
    ## 12 Multimodal     Side                 1758

### Between-Condition Observational Differences

We’ll first evaluate the differences in the number of observation days
and sales counts across study periods.

- Differences in the number of observation days across menu conditions:

menu_condition

``` r
fall_data %>%
  group_by(menu_condition,date) %>%
  summarise(count=n()) %>%
  mutate(day_count=1) %>%
  summarise(observation_days=sum(day_count))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 2
    ##   menu_condition observation_days
    ##   <chr>                     <dbl>
    ## 1 Carbon Label                 10
    ## 2 Control                       8
    ## 3 Default                      10
    ## 4 Multimodal                   17

- Differences in global sales volume across menu conditions:

``` r
fall_data %>%
  group_by(menu_condition,date) %>%
  summarise(count=n()) %>%
  group_by(menu_condition) %>%
  summarise(sales_count=sum(count))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 4 × 2
    ##   menu_condition sales_count
    ##   <chr>                <int>
    ## 1 Carbon Label           505
    ## 2 Control                409
    ## 3 Default                499
    ## 4 Multimodal             801

- Daily Sales volume

### Grill - Mains - Prop

``` r
fall_data %>%
  filter(menu_condition=="Control") %>%
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
  filter(menu_condition=="Carbon Label") %>%
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
  filter(menu_condition=="Default") %>%
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
  filter(menu_condition=="Multimodal") %>%
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
    ## 1 Black Bean Burger                        41 0.0486 Multimodal
    ## 2 Grilled Chicken Breast Sandwich         165 0.195  Multimodal
    ## 3 Grilled Hamburger                      1157 1.37   Multimodal
    ## 4 Seared Salmon Burger                    125 0.148  Multimodal
    ## 5 Trillium Grill Impossible Burger        107 0.127  Multimodal

``` r
fall_data %>%
  filter(menu_condition=="Multimodal (Extra)") %>%
  filter(station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(14+81+533+66+57)) %>%
  mutate(condition="Multimodal (Extra)")
```

    ## # A tibble: 0 × 4
    ## # ℹ 4 variables: item <chr>, item_count <int>, prop <dbl>, condition <chr>

``` r
fall_data %>%
  filter(menu_condition=="Multimodal (Extra)"| menu_condition=="Multimodal") %>%
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
  filter(menu_condition=="Control") %>%
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
  filter(menu_condition=="Carbon Label") %>%
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
  filter(menu_condition=="Default") %>%
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
  filter(menu_condition=="Multimodal") %>%
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
    ## 1 Bowl Ramen Chicken        860 1.45  Multimodal
    ## 2 Bowl Ramen Tofu           182 0.307 Multimodal

``` r
fall_data %>%
  filter(menu_condition=="Multimodal (Extra)") %>%
  filter(station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  group_by(item) %>%
  summarise(item_count=sum(count)) %>%
  mutate(prop=item_count/(377+73)) %>%
  mutate(condition="Multimodal (Extra)")
```

    ## # A tibble: 0 × 4
    ## # ℹ 4 variables: item <chr>, item_count <int>, prop <dbl>, condition <chr>

``` r
fall_data %>%
  filter(menu_condition=="Multimodal (Extra)" | menu_condition=="Multimodal") %>%
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
  filter(menu_condition=="Control") %>%
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

CONSIDER distance between highest-carbon offering and next-lowest
offering between stations Price relativity Number of veg options

``` r
fall_data %>%
  distinct(date)
```

    ##      date
    ## 1  16-Oct
    ## 2  17-Oct
    ## 3  18-Oct
    ## 4  21-Oct
    ## 5  22-Oct
    ## 6  23-Oct
    ## 7  24-Oct
    ## 8  25-Oct
    ## 9  28-Oct
    ## 10 29-Oct
    ## 11 30-Oct
    ## 12 31-Oct
    ## 13  1-Nov
    ## 14  4-Nov
    ## 15  5-Nov
    ## 16  6-Nov
    ## 17  7-Nov
    ## 18  8-Nov
    ## 19 11-Nov
    ## 20 12-Nov
    ## 21 13-Nov
    ## 22 14-Nov
    ## 23 15-Nov
    ## 24 18-Nov
    ## 25 19-Nov
    ## 26 20-Nov
    ## 27 21-Nov
    ## 28 22-Nov
    ## 29 25-Nov
    ## 30 26-Nov
    ## 31  2-Dec
    ## 32  3-Dec
    ## 33  4-Dec
    ## 34  5-Dec
    ## 35  6-Dec
    ## 36  9-Dec
    ## 37 10-Dec
    ## 38 11-Dec
    ## 39 12-Dec
    ## 40 13-Dec
    ## 41 16-Dec
    ## 42 17-Dec
    ## 43 18-Dec
    ## 44 19-Dec
    ## 45 20-Dec

``` r
spring_data %>%
  distinct(date)
```

    ##      date
    ## 1  21-Jan
    ## 2  22-Jan
    ## 3  23-Jan
    ## 4  24-Jan
    ## 5  27-Jan
    ## 6  28-Jan
    ## 7  29-Jan
    ## 8  30-Jan
    ## 9  31-Jan
    ## 10  3-Feb
    ## 11  4-Feb
    ## 12  5-Feb
    ## 13  6-Feb
    ## 14  7-Feb
    ## 15 10-Feb
    ## 16 11-Feb
    ## 17 12-Feb
    ## 18 13-Feb
    ## 19 14-Feb
    ## 20 19-Feb
    ## 21 20-Feb
    ## 22 21-Feb
    ## 23 24-Feb
    ## 24 25-Feb
    ## 25 26-Feb
    ## 26 27-Feb
    ## 27 28-Feb
    ## 28  3-Mar
    ## 29  4-Mar
    ## 30  5-Mar
    ## 31  6-Mar
    ## 32  7-Mar
    ## 33 10-Mar
    ## 34 11-Mar
    ## 35 12-Mar
    ## 36 13-Mar
    ## 37 14-Mar
    ## 38 17-Mar
    ## 39 18-Mar
    ## 40 19-Mar
    ## 41 20-Mar
    ## 42 21-Mar
    ## 43 24-Mar
    ## 44 25-Mar
    ## 45 26-Mar
    ## 46 27-Mar
    ## 47 28-Mar
    ## 48  7-Apr
    ## 49  8-Apr
    ## 50  9-Apr
    ## 51 10-Apr
    ## 52 11-Apr
    ## 53 14-Apr
    ## 54 15-Apr
    ## 55 16-Apr
    ## 56 17-Apr
    ## 57 18-Apr

``` r
fall_data %>%
  filter(station=="Grill"|station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  summarise(sum(count))
```

    ##   sum(count)
    ## 1       8601

``` r
spring_data %>%
  filter(station=="Grill"|station=="Ramen") %>%
  filter(item_cat=="Main") %>%
  summarise(sum(count))
```

    ##   sum(count)
    ## 1      11873

differnce in carbon estiamte for highest and lowest number of options to
choose between

Sides and modifications not factored in currently

also measure spillover across other stations during same meal interval

measure spillover across other meal interval

### Foot Traffic

``` r
foot_traffic_data %>%
  select(date,time,count,menu_condition) %>%
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
  select(date,time,count,menu_condition) %>%
  group_by(menu_condition,time) %>%
  summarise(transaction_volume=mean(count)) 
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 98 × 3
    ## # Groups:   menu_condition [3]
    ##    menu_condition time     transaction_volume
    ##    <chr>          <chr>                 <dbl>
    ##  1 Control        10:00 AM              44.5 
    ##  2 Control        10:15 AM              27.7 
    ##  3 Control        10:30 AM               8.82
    ##  4 Control        10:45 AM              14.0 
    ##  5 Control        11:00 AM              97.6 
    ##  6 Control        11:15 AM              91.5 
    ##  7 Control        11:30 AM             126.  
    ##  8 Control        11:45 AM              88.4 
    ##  9 Control        12:00 PM             106.  
    ## 10 Control        12:15 PM             140.  
    ## # ℹ 88 more rows

``` r
head(5)
```

    ## [1] 5

``` r
foot_traffic_data %>%
  select(date,time,count,menu_condition) %>%
  group_by(menu_condition,time) %>%
  summarise(transaction_volume=mean(count)) %>%
  ggplot(aes(x=time,y=transaction_volume,fill=menu_condition)) + 
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("6:15 AM","6:30 AM","6:45 AM","7:00 AM","7:15 AM","7:30 AM","7:45 AM","8:00 AM","8:15 AM","8:30 AM","8:45 AM","9:00 AM","9:15 AM","9:30 AM","9:45 AM","10:00 AM","10:15 AM","10:30 AM","10:45 AM","11:00 AM","11:15 AM","11:30 AM","11:45 AM","12:00 PM","12:15 PM","12:30 PM","12:45 PM","1:00 PM","1:15 PM","1:30 PM","1:45 PM","2:00 PM","2:15 PM","2:30 PM","2:45 PM","3:00 PM","3:15 PM","3:30 PM","3:45 PM","4:00 PM"))
```

    ## `summarise()` has grouped output by 'menu_condition'. You can override using
    ## the `.groups` argument.

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-183-1.png)<!-- -->

sales_data %\>% mutate(item_cat=case_when(item==“Quesadilla Deluxe
Trillium”~“Main”, item==“Grilled Hamburger”~“Main”, item==“Fried Chicken
Tenders”~“Main”, item==“Burrito Una Mano Trillium BYO”~“Main”,
item==“French Fries”~“Side”, item==“Quesadilla Cheese”~“Ma
