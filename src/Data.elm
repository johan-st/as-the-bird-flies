module Data exposing (..)


toDisplayList =
    List.repeat 10
        { airlineId = 1
        , airline = "SUS"
        , origin =
            { id = 1
            , name = "Arlanda"
            , city = "Stockholm"
            , location =
                ( 59.651901245117
                , 17.918600082397
                )
            }
        , destinatin =
            { id = 2
            , name = "Oslo Lufthavn"
            , city = "Oslo"
            , location =
                ( 60.121
                , 11.0502
                )
            }
        , distance = 99
        }


arlanda =
    { id = 1
    , name = "Arlanda"
    , city = "Stockholm"
    , location =
        ( 59.651901245117
        , 17.918600082397
        )
    }
