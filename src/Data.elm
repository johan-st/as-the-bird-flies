module Data exposing (..)


toDisplayList =
    List.repeat 10
        { airlineId = 1
        , airline = "SUS"
        , origin = 1
        , destination = 2
        , distance = 0
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


gardemoen =
    { id = 2
    , name = "gardemoen"
    , city = "Oslo"
    , location =
        ( 60.121
        , 11.0502
        )
    }
