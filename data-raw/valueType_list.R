## code to prepare `valueType_list` dataset goes here
valueType_list <-
  dplyr::tribble(
    ~`valueType`,~`typeof`  ,~`class`        ,~`call`         ,~`toValueType`,~`toTypeof`,~`genericType`,
    NA          ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "text"      ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "integer"   ,"integer"  ,"integer"       ,"as.integer"    ,"integer"     ,"integer"  , "numeric"    ,
    "decimal"   ,"double"   ,"numeric"       ,"as.numeric"    ,"decimal"     ,"double"   , "numeric"    ,
    "boolean"   ,"logical"  ,"logical"       ,"as_any_boolean","boolean"     ,"logical"  , "numeric"    ,
    "locale"    ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "datetime"  ,"double"   ,"POSIXct,POSIXt","as.character"  ,"text"        ,"character", "character"  ,
    "date"      ,"double"   ,"Date"          ,"as_any_date"   ,"decimal"     ,"double"   , "date"       ,
    "binary"    ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "point"     ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "linestring","character","character"     ,"as.character"  ,"text"        ,"character", "character"  ,
    "polygon"   ,"character","character"     ,"as.character"  ,"text"        ,"character", "character"
  )
usethis::use_data(valueType_list, overwrite = TRUE)
