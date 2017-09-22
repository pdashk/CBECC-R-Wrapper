#put config file and XML file in the same directory!!
#do not include extension ("test" instead of "test.xml")
base_xml <- "1StoryExample3EDR"

# some key assumptions:
# floor area affects four parameters: attic area, coditioned floor area, slab area, slab perimeter
#### attic area = floor area / # stories
#### conditioned floor area = floor area
#### slab area = attic area
#### slab perimeter = sqrt(slab area)*4
#### ...
# Gas = natural gas, which is available in the home even if both appliances are electric
# Address and zipcode will be defaulted to "default" and 0, respectively.
#### Address does not appear to affect calculations, but will take this precaution anyway

newVars <- list(
    `b$Proj$Zone$FloorArea` = seq(400,1400,500),
    `b$Proj$NumBedrooms` = c(1:2),
    `b$Proj$ClimateZone` = c("CZ1  (Arcata)",
                             "CZ2  (Santa Rosa)",
                             "CZ3  (Oakland)",
                             "CZ4  (San Jose)",
                             "CZ5  (Santa Maria)",
                             "CZ6  (Torrance)",
                             "CZ7  (San Diego)",
                             "CZ8  (Fullerton)",
                             "CZ9  (Burbank)",
                             "CZ10  (Riverside)",
                             "CZ11  (Red Bluff)",
                             "CZ12  (Sacramento)",
                             "CZ13  (Fresno)",
                             "CZ14  (Palmdale)",
                             "CZ15  (Palm Springs)",
                             "CZ16  (Blue Canyon)"),
    `b$Proj$ApplCookFuel` = c("Gas","Electricity"),
    `b$Proj$FrontOrientation` = c(0,90,180,270),
    `b$Proj$ApplDryerFuel` = c("Gas","Electricity")
)


