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
  filter(station=="Grill" | station=="Ramen") %>%
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
    ## 16               ADD Chicken Breast
    ## 17               Add Extra Toppings

We’ll begin by pairing the calculated water- and carbon-footprint
estimates to each corresponding composite item (i.e., an item comprised
of more than one ingredient, as defined by the research partner’s recipe
system):

``` r
composite_item_pairing <- ingredient_level_footprint_data %>%
  group_by(recipe) %>%
  summarise(ind_carbon_cost=sum(kg_co2e),ind_water_cost=sum(l_h2o_blue)) %>%
  filter(recipe=="[CYO Grill Component] Burger" | recipe=="[CYO Grill Component] Fries" | recipe=="[CYO Grill Component] Chicken Sandwich" | recipe=="[CYO Grill Component] Salmon Burger" | recipe=="[CYO Grill Component] Impossible Burger" | recipe=="[CYO Grill Component] Sweet Potato Fries" | recipe=="[CYO Grill Component] Black Bean Burger" | recipe=="[CYO Ramen Component] Chicken Bowl" | recipe=="[CYO Ramen Component] Tofu Bowl" | recipe=="[CYO Ramen Component] Ginger Tare") %>%
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
  filter(recipe=="[CYO Grill Component] Toppings, American Cheese" | recipe=="[CYO Grill Component] Toppings, Cheddar Cheese" | recipe=="[CYO Grill Component] Toppings, Pepper Jack Cheese" | recipe=="[CYO Grill Component] Toppings, Provolone Cheese" | recipe=="[CYO Grill Component] Toppings, Blue Cheese" | recipe=="[CYO Grill Component] Toppings, Vegan Cheese") %>%
  select(recipe,kg_co2e,l_h2o_blue) %>% 
  mutate(item="ADD Cheese") %>%
  group_by(item) %>%
  summarise(ind_carbon_cost=mean(kg_co2e),ind_water_cost=mean(l_h2o_blue))
```

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
associated with each of the items offered at the treatment station, we
can perform a join to incorporate this information into `sales_data`.
Before that, though, we need to add the monetary costs of each changes,
too, to help us understand whether sales revenue varied across the
different menu conditions trialled.

While we have most of the pricing information available to accomplish
this, we assume a flat 2.99 fee for all additional patty modifications,
with the exception of the “Add Sausage 2 Patty” modification, which
instead maps on to the bacon addition presented on the “Grill” menu. We
also assume a \$0.99 fee “Add Extra Toppings” modification at the
“Ramen” station, which maps on to the Ginger Tare that appears in our
recipe modeling.

``` r
item_pairing <- bind_rows(composite_item_pairing,variable_item_pairing,simple_item_pairing)
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
                                  item=="Add Egg .99"~.99))
item_pairing
```

    ## # A tibble: 17 × 4
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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->
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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

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
fall_vline <- as.Date(c("2024-10-28","2024-11-11","2024-11-25"))
fall_vline <- which(daily_prop_low_fall_data$date %in% fall_vline)
```

``` r
ggplot(daily_prop_low_fall_data,aes(x=date,y=prop_low,color=station)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
grill_prop_low <- fall_data %>%
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
  ggplot(aes(x=date,y=prop_low,color=item)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Paired",name="") +
  scale_y_continuous(breaks=c(0.00,0.01,0.02,0.03,0.04,0.05)) +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=0.04,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.04,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.04,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.04,label="Multimodal",size=10/.pt) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
ggsave(filename="prop_low.png",plot=grill_prop_low,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_low
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
ramen_prop_low <- fall_data %>%
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
  ggplot(aes(x=date,y=prop_low,color=item)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Paired",name="") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=0.25,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.25,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.25,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.25,label="Multimodal",size=10/.pt) 
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.

``` r
  ggsave(filename="ramen_prop_low.png",plot=ramen_prop_low,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
ramen_prop_low
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
treatment_prop_low <- fall_data %>%
  filter(station=="Ramen"|station=="Grill") %>%
  filter(item_cat=="Main") %>%
  group_by(date,menu_condition,station,item) %>%
  summarise(total_count=sum(count)) %>%
  mutate(item=case_when(item=="Bowl Ramen Tofu"~"Lowest-Carbon Offering",
                        item=="Black Bean Burger"~"Lowest=Carbon Offering",
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
  ggplot(aes(x=date,y=prop_low,color=item)) + 
  geom_smooth() + 
  geom_point() +
  geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") +
  scale_color_brewer(palette="Paired",name="") +
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=0.12,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.12,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.12,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.12,label="Multimodal",size=10/.pt)
```

    ## `summarise()` has grouped output by 'date', 'menu_condition', 'station'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'date', 'menu_condition'. You can override
    ## using the `.groups` argument.

``` r
ggsave(filename="treatment_prop_low.png",plot=treatment_prop_low,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
treatment_prop_low
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
grill_prop_high <- daily_prop_high_fall_data %>%
  filter(item=="Grilled Hamburger") %>%
  ggplot(aes(x=date,y=prop_high,color=item)) + 
           geom_smooth() + 
           geom_point() +
           geom_vline(xintercept=as.numeric(daily_prop_low_fall_data$date[fall_vline]),linetype=2) +
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) +
  annotate("text",x=as.Date("2024-10-20"),y=0.79,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-4"),y=0.79,label="Carbon Label",size=10/.pt) +
  annotate("text",x=as.Date("2024-11-18"),y=0.79,label="Default",size=10/.pt) +
  annotate("text",x=as.Date("2024-12-7"),y=0.79,label="Multimodal",size=10/.pt) 
ggsave(filename="prop_high.png",plot=grill_prop_high,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_high
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

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
ggsave(filename="prop_middle.png",plot=grill_prop_mid,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_mid
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

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
ggsave(filename="mean_carbon.png",plot=grill_mean_carbon_cost,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_mean_carbon_cost
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

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
ggsave(filename="mean_spend.png",plot=mean_spend,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
mean_spend
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

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
spring_vline <- as.Date(c("2025-01-21","2025-02-03","2025-02-24","2025-03-10","2025-03-24","2024-04-18")) 
spring_vline <- which(daily_prop_spring_data$date %in% spring_vline)
```

``` r
grill_prop_low_s2 <- daily_prop_spring_data %>%
  filter(item=="Black Bean Burger") %>%
  ggplot(aes(x=date,y=prop,color=item)) + 
  geom_smooth() +
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  scale_color_brewer(palette="Set2") + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) + 
  annotate("text",x=as.Date("2025-01-27"),y=0,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0,label="Control",size=10/.pt) 
ggsave(filename="s2_prop_low.png",plot=grill_prop_low_s2,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_low_s2
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
grill_prop_high_s2 <- daily_prop_spring_data %>%
  filter(item=="Grilled Hamburger") %>%
  ggplot(aes(x=date,y=prop,color=item)) + 
  geom_smooth() +
  geom_point() + 
  geom_vline(xintercept=as.numeric(daily_prop_spring_data$date[spring_vline]),linetype=2) + 
  scale_color_brewer(palette="Set2") + 
  xlab("Date") + 
  ylab("Proportion of Station Sales") + 
  theme(aspect.ratio=0.55,legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10)) + 
  annotate("text",x=as.Date("2025-01-27"),y=0.6,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0.6,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0.6,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0.6,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0.6,label="Control",size=10/.pt) 
ggsave(filename="s2_prop_high.png",plot=grill_prop_high_s2,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_low_s2
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

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
  annotate("text",x=as.Date("2025-01-27"),y=0,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-02-14"),y=0,label="Multimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-03"),y=0,label="Control",size=10/.pt) +
  annotate("text",x=as.Date("2025-03-17"),y=0,label="Unimodal",size=10/.pt) +
  annotate("text",x=as.Date("2025-04-06"),y=0,label="Control",size=10/.pt) 
ggsave(filename="s2_prop_middle.png",plot=grill_prop_middle_s2,path="/Users/kenjinchang/github/multimodal-framework-validation/figures",width=30,height=20,units="cm",dpi=150,limitsize=TRUE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

``` r
grill_prop_low_s2
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

We’ll need to do checks to see whether there are differences in the
purchase of different item categories across menu conditions, whether
there are differences in the sales total across menu conditions,
differences in the number of observation days across menu conditions,
differences in the daily sales average across menu conditions

Spillover within meal period and between meal period

also look at diminishing effects over time by looking at outcomes at day
level, not just by period

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

![](cleaning-and-analysis_files/figure-gfm/unnamed-chunk-97-1.png)<!-- -->

sales_data %\>% mutate(item_cat=case_when(item==“Quesadilla Deluxe
Trillium”~“Main”, item==“Grilled Hamburger”~“Main”, item==“Fried Chicken
Tenders”~“Main”, item==“Burrito Una Mano Trillium BYO”~“Main”,
item==“French Fries”~“Side”, item==“Quesadilla Cheese”~“Ma
