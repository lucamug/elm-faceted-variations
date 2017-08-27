module Main exposing (..)

{-
   To run this test:

   $ elm-live src/main.elm --open --debug
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Array
import Time
import Task
import Json.Decode
import Json.Decode.Pipeline


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type alias Model =
    { variantSelected : List String
    , variants : Dict.Dict String VariantCombination
    , payloadJson : String
    , payload : Maybe Payload
    , error : Maybe String
    }


type alias VariantType =
    { attribute : String
    , variantValues : VariantValues
    }


type alias Variant =
    { id : String
    , variantValues : VariantValues
    , price : Float
    , available : Int
    }


type alias VariantValues =
    List String


type alias Payload =
    { variantsTypes : List VariantType
    , variants : List Variant
    }


type alias VariantCombination =
    { minPrice : Float
    , maxPrice : Float
    , available : Int
    , ids : List String
    }


type Msg
    = NoOp
    | SwitchTo ( Int, String )
    | InitialiseVariants Time.Time
    | DecodePayload Time.Time
    | UpdatePayload String


decodePayload : Json.Decode.Decoder Payload
decodePayload =
    Json.Decode.Pipeline.decode Payload
        |> Json.Decode.Pipeline.required "variantsTypes" (Json.Decode.list decodePayloadVariantsTypes)
        |> Json.Decode.Pipeline.required "variants" (Json.Decode.list decodePayloadVariants)


decodePayloadVariantsTypes : Json.Decode.Decoder VariantType
decodePayloadVariantsTypes =
    Json.Decode.Pipeline.decode VariantType
        |> Json.Decode.Pipeline.required "attribute" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "variantValues" (Json.Decode.list Json.Decode.string)


decodePayloadVariants : Json.Decode.Decoder Variant
decodePayloadVariants =
    Json.Decode.Pipeline.decode Variant
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "variantValues" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "price" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "available" (Json.Decode.int)


viewItem : Model -> Bool -> String -> String -> Int -> Html Msg
viewItem model checkedStatus attribute value index =
    let
        newVariantSelected =
            joiner (listSet index value model.variantSelected)

        presentData =
            Dict.get newVariantSelected model.variants

        disabledStatus =
            presentData == Nothing
    in
        div
            [ classList
                [ "viewItem" => True
                , "disabled" => disabledStatus
                , "checked" => checkedStatus
                ]
            , if disabledStatus then
                onClick (NoOp)
              else if checkedStatus then
                onClick (SwitchTo ( index, "❌" ))
              else
                onClick (SwitchTo ( index, value ))
            ]
            [ text value
            , span [ class "right" ] [ text (formatPriceAndAvailability presentData) ]
            ]


formatPrice : a -> String
formatPrice price =
    (toString price) ++ " EUR"


formatPriceAndAvailability : Maybe { b | minPrice : a, maxPrice : a } -> String
formatPriceAndAvailability data =
    case data of
        Just data2 ->
            -- data2
            if data2.minPrice == data2.maxPrice then
                (formatPrice data2.minPrice)
            else
                (toString data2.minPrice) ++ " ~ " ++ (formatPrice data2.maxPrice)

        Nothing ->
            "Out of stock"


viewItemSection : Model -> VariantType -> Int -> List (Html Msg)
viewItemSection model section index =
    let
        checked =
            Array.get index (Array.fromList model.variantSelected)
    in
        [ h3 []
            [ section.attribute
                |> text
            ]
        ]
            ++ (List.map
                    (\item -> viewItem model (checked == Just item) section.attribute item index)
                    (section.variantValues)
               )


viewJsonForm : Model -> Html Msg
viewJsonForm model =
    div []
        [ button [ onClick (DecodePayload 0) ] [ text "Apply this payload" ]
        , textarea [ onInput UpdatePayload ] [ text model.payloadJson ]
        ]


view : Model -> Html Msg
view model =
    case model.payload of
        Nothing ->
            div []
                [ p [] [ text "No Payload" ]
                , p [] [ text (toString model.error) ]
                ]

        Just payload ->
            let
                presentSelection =
                    (joiner model.variantSelected)

                presentData =
                    Dict.get presentSelection model.variants

                variationsQuantity =
                    case presentData of
                        Just data ->
                            List.length data.ids

                        Nothing ->
                            0

                ctaDisabled =
                    case presentData of
                        Just data ->
                            (List.length data.ids) > 1

                        Nothing ->
                            True
            in
                div
                    [ class "main" ]
                    [ node "style" [] [ text css ]
                    , div [ class "header" ]
                        [ h1 []
                            [ presentData |> formatPriceAndAvailability |> text
                            ]
                        , p [] [ ((variationsQuantity |> toString) ++ " variants") |> text ]
                        , button
                            [ classList
                                [ "cta" => True
                                , "disabled" => ctaDisabled
                                ]
                            , disabled ctaDisabled
                            ]
                            [ text "Add to Cart" ]
                        ]
                    , div [ class "body" ]
                        (List.concat (List.indexedMap (\index item -> (viewItemSection model item index)) payload.variantsTypes))
                    , div [ class "footer" ]
                        [ case model.error of
                            Nothing ->
                                text ""

                            Just data ->
                                div [ class "error" ]
                                    [ h4 [] [ "Error" |> text ]
                                    , p [] [ text data ]
                                    ]
                        ]
                    ]


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


initModel : Model
initModel =
    { variantSelected = []
    , variants = Dict.empty
    , error = Nothing
    , payloadJson = json
    , payload = Nothing
    }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ Task.perform DecodePayload Time.now
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


listSet : Int -> String -> List String -> List String
listSet index value variantSelected =
    Array.toList (Array.set index value (Array.fromList variantSelected))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SwitchTo variant ->
            let
                ( index, value ) =
                    variant

                newVariantSelected =
                    listSet index value model.variantSelected
            in
                ( { model | variantSelected = newVariantSelected }, Cmd.none )

        DecodePayload time ->
            let
                decoded =
                    Json.Decode.decodeString decodePayload model.payloadJson
            in
                case decoded of
                    Ok data ->
                        ( { model | error = Nothing, payload = Just data }, Task.perform InitialiseVariants Time.now )

                    Err data ->
                        ( { model | error = Just data }, Cmd.none )

        InitialiseVariants time ->
            case model.payload of
                Nothing ->
                    ( model, Cmd.none )

                Just payload ->
                    let
                        _ =
                            Debug.log "after" (prepareVariantValues payload.variants)

                        _ =
                            Debug.log "before" payload.variants
                    in
                        ( { model
                            | variants =
                                payload.variants
                                    |> prepareVariantValues
                                    |> List.concat
                                    |> updateDict Dict.empty
                            , variantSelected =
                                ((payload.variantsTypes
                                    |> List.length
                                    |> flip List.repeat "❌"
                                 )
                                )
                          }
                        , Cmd.none
                        )

        UpdatePayload data ->
            ( { model | payloadJson = data }, Cmd.none )



{-
   prepareVariantValues

   Convert a structure of this type

    [ { id = "0", variantValues = ["Red","Small"], price = 1.99, available = 1 }
    , { id = "1", variantValues = ["Green","Small"], price = 2.99, available = 10 }
    , { id = "2", variantValues = ["Red","Large"], price = 3.99, available = 100 }
    ]

   Into this

    [
        [ ("❌:❌",{ id = "0", variantValues = ["Red","Small"], price = 1.99, available = 1 })
        , ("❌:Small",{ id = "0", variantValues = ["Red","Small"], price = 1.99, available = 1 })
        , ("Red:❌",{ id = "0", variantValues = ["Red","Small"], price = 1.99, available = 1 })
        , ("Red:Small",{ id = "0", variantValues = ["Red","Small"], price = 1.99, available = 1 })
        ]
    ,
        [ ("❌:❌",{ id = "1", variantValues = ["Green","Small"], price = 2.99, available = 10 })
        , ("❌:Small",{ id = "1", variantValues = ["Green","Small"], price = 2.99, available = 10 })
        , ("Green:❌",{ id = "1", variantValues = ["Green","Small"], price = 2.99, available = 10 })
        , ("Green:Small",{ id = "1", variantValues = ["Green","Small"], price = 2.99, available = 10 })
        ]
    ,
        [ ("❌:❌",{ id = "2", variantValues = ["Red","Large"], price = 3.99, available = 100 })
        , ("❌:Large",{ id = "2", variantValues = ["Red","Large"], price = 3.99, available = 100 })
        , ("Red:❌",{ id = "2", variantValues = ["Red","Large"], price = 3.99, available = 100 })
        , ("Red:Large",{ id = "2", variantValues = ["Red","Large"], price = 3.99, available = 100 })
        ]
    ]
-}


prepareVariantValues : List Variant -> List (List ( String, Variant ))
prepareVariantValues variants =
    List.map
        (\variant ->
            variant.variantValues
                |> listToListOfList
                |> generateVariantsCombination []
                |> transform1 variant
        )
        variants



{-
   Convert a list of any type in a List of List of any type
-}


listToListOfList : List a -> List (List a)
listToListOfList list =
    List.map (\item -> [ item ]) list


joiner : List String -> String
joiner list =
    String.join ":" list



{-
   generateVariantsCombination

   This function take a generic list of lists. For example

   [ [ "1", "2" ], [ "a", "b" ] ]

   And it returns and ordered permutation of all values of each list.
   For the example above, this would be the resault:

   [ "1:a", "1:b", "2:a", "2:b" ]

   It is a recursive function. It starts from the first list and for each
   item, pass to the second list and so on. During the execution it stores
   temporary values in "temp"

   "temp" is ["1"] at the beginning, the is ["1", "a"]. At this point
   the "head" will be "Nothing" so the temp is joined with ":" and returned
   as element of a list: ["1:a"]

   For the "tail" is necessary always to return an empyt list also when
   the tail is not present.

   Use an empy list to call this function, as second parameter:

   generateVariantsCombination [ [ "1", "2" ], [ "a", "b" ] ] []
-}


generateVariantsCombination : List String -> List (List String) -> List String
generateVariantsCombination acc list =
    case List.head list of
        Nothing ->
            [ joiner (List.reverse acc) ]

        Just headList ->
            let
                tail =
                    case List.tail list of
                        Just data ->
                            data

                        Nothing ->
                            []
            in
                List.concat
                    (List.map
                        (\item ->
                            generateVariantsCombination (item :: acc) (tail)
                        )
                        ("❌" :: headList)
                    )



{-
   This function take a structure of this type (list)

   for example list = [ "1:a", "1:b", "2:a", "2:b" ]

   and something else of any type (object)

   fo example object = "product01"

   and combine them together like this,


   [ ("1:a", "product01")
   , ("1:b", "product01")
   , ("2:a", "product01")
   , ("2:b", "product01")
   ]

-}


transform1 : b -> List a -> List ( a, b )
transform1 object list =
    List.map (\item -> ( item, object )) list



{-
   updateDict

   This is the main loop to load all data in a Dict structure. For each item
   in the list it execute addToDict to check if the dict already has such variant
-}


updateDict : Dict.Dict comparable VariantCombination -> List ( comparable, Variant ) -> Dict.Dict comparable VariantCombination
updateDict dict list =
    -- [ ( "25", "Alice" ), ( "25", "Bob" ), ( "31", "Robert" ) ]
    List.foldl (\item dict -> addToDict item dict) dict list



{-
   addToDict

   add a new variant (variant) that has certain key (key) to the list of all
   variants. If the key already exsist, it calculate the new minPrice,
   maxPrice, available and ids.

   If not, it just create a new entry in the Dict

-}


addToDict : ( comparable, Variant ) -> Dict.Dict comparable VariantCombination -> Dict.Dict comparable VariantCombination
addToDict ( key, variant ) dict =
    case Dict.get key dict of
        Just data ->
            -- The combination alread exsists
            Dict.insert
                key
                { data
                    | minPrice = Basics.min data.minPrice variant.price
                    , maxPrice = Basics.max data.maxPrice variant.price
                    , available = data.available + variant.available
                    , ids = variant.id :: data.ids
                }
                dict

        Nothing ->
            -- The combination is new
            Dict.insert
                key
                (VariantCombination variant.price variant.price variant.available [ variant.id ])
                dict


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


css : String
css =
    """
body {
    color: #888;
    margin: 0;
    font-family: sans-serif;
    background-color: #ddd;
}
.main {
    max-width: 800px;
    margin: 0 auto;
    box-shadow: 0px 5px 20px 0px hsla(0, 0%, 0%, 0.16);
}
.header, .body, .footer {
    padding: 12px;
}
.header {
    background-color: white;
    position: fixed;
    border-bottom: 1px solid #ddd;
    max-width: 776px;
    width: 100%;
    box-shadow: 0px 5px 20px 0px hsla(0, 0%, 0%, 0.16);
}
.body {
    background-color: #eee;
    padding-top: 100px;
}
.footer {
    background-color: #eee;
    color: #aaa;
}
.viewItem {
    background-color: white;
    padding: 6px;
    margin: 6px 0;
    cursor: pointer;
    border: 3px solid white;
}
.cta {
    background-color: #82c2e2;
    color: white;
    position: absolute;
    top: 12px;
    right: 12px;
    font-size: 1.0em;
    border-width: 0;
    padding: 12px 24px;
    border-radius: 25px;
}
.cta.disabled {
    background-color: #ddd;
    color: #aaa;
}
.viewItem.disabled {
    background-color: #ddd;
    border-color: #ddd;
    color: #ccc;
    cursor: not-allowed;
}
.checked {
    background-color: #e4f3fb;
    border-color: #82c2e2;
}
h1 {
    font-size: 1.2em;
}
h3 {
    margin: 16px 0 0 6px;
    color: #888;
}
.right {
    float: right;
}
textarea {
    font-family: monospace;
    width: 100%;
    font-size: 1em;
    height: 600px;
}
.error {
    color: red;
}
"""


json : String
json =
    """
{
    "variantsTypes": [
        {
            "attribute": "Color",
            "variantValues": [
                "Red",
                "Green"
            ]
        },
        {
            "attribute": "Size",
            "variantValues": [
                "Small",
                "Medium",
                "Large"
            ]
        }
    ],
    "variants": [
        {
            "id": "0",
            "variantValues": [
                "Red",
                "Small"
            ],
            "price": 1.99,
            "available": 1
        },
        {
            "id": "1",
            "variantValues": [
                "Green",
                "Small"
            ],
            "price": 2.99,
            "available": 10
        },
        {
            "id": "2",
            "variantValues": [
                "Red",
                "Large"
            ],
            "price": 3.99,
            "available": 100
        }
    ]
}
"""
