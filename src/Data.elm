module Data exposing (..)


toDisplayList =
    List.repeat 10
        { airlineId = "NaN-AIR"
        , flightNr = "Nr9999"
        , origin =
            { id = "ARN"
            , name = "Arlanda"
            , city = "Stockholm"
            , location =
                ( 59.651901245117
                , 17.918600082397
                )
            }
        , destinatin =
            { id = "OSL"
            , name = "Oslo Lufthavn"
            , city = "Oslo"
            , location =
                ( 60.121
                , 11.0502
                )
            }
        }
