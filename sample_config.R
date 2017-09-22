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
    `b$Proj$Zone$FloorArea` = 400,
    `b$Proj$NumBedrooms` = c(1:2)
)


