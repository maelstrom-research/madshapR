## code to prepare `valueType_list` dataset goes here
valueType_list <-
  dplyr::bind_cols(
    dplyr::tribble(
      ~`valueType`,~`typeof`  ,~`class`        ,~`call`         ,
      NA          ,"character","character"     ,"as.character.default"  ,
      "text"      ,"character","character"     ,"as.character.default"  ,
      "integer"   ,"integer"  ,"integer"       ,"as.integer"    ,
      "decimal"   ,"double"   ,"numeric"       ,"as.numeric"    ,
      "boolean"   ,"logical"  ,"logical"       ,"as_any_boolean",
      "locale"    ,"character","character"     ,"as.character.default"  ,
      "datetime"  ,"double"   ,"POSIXct,POSIXt","as.character.default"  ,
      "date"      ,"double"   ,"Date"          ,"as_any_date"   ,
      "binary"    ,"character","character"     ,"as.character.default"  ,
      "point"     ,"character","character"     ,"as.character.default"  ,
      "linestring","character","character"     ,"as.character.default"  ,
      "polygon"   ,"character","character"     ,"as.character.default"
    ),
    dplyr::tribble(
      ~`toValueType`,~`toTypeof`,~`genericType`,
      "text"        ,"character", "character"  ,
      "text"        ,"character", "character"  ,
      "integer"     ,"integer"  , "numeric"    ,
      "decimal"     ,"double"   , "numeric"    ,
      "boolean"     ,"logical"  , "numeric"    ,
      "text"        ,"character", "character"  ,
      "text"        ,"character", "character"  ,
      "decimal"     ,"double"   , "date"       ,
      "text"        ,"character", "character"  ,
      "text"        ,"character", "character"  ,
      "text"        ,"character", "character"  ,
      "text"        ,"character", "character"
    )
  )
usethis::use_data(valueType_list, overwrite = TRUE)
