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
import Round


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


emptyVariant : Variant
emptyVariant =
    { id = ""
    , variantValues = []
    , price = 0
    , available = 0
    }


type alias VariantValues =
    List String


type alias Payload =
    { variantsTypes : List VariantType
    , variants : List Variant
    }


emptyPayload : Payload
emptyPayload =
    { variantsTypes = []
    , variants = []
    }


type alias VariantCombination =
    { minPrice : Float
    , maxPrice : Float
    , available : Int
    , ids : List String
    }


emptyVariantCombination : VariantCombination
emptyVariantCombination =
    { minPrice = 0
    , maxPrice = 0
    , available = 0
    , ids = []
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

        disabledStatus =
            variantCombination.available <= 0

        variantCombination =
            case Dict.get newVariantSelected model.variants of
                Just data ->
                    data

                Nothing ->
                    emptyVariantCombination
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
            , span [ class "right" ] [ text (formatPriceAndAvailability variantCombination) ]
            ]


formatPrice : Float -> String
formatPrice price =
    "$" ++ (Round.round 2 price)


formatPriceAndAvailability : VariantCombination -> String
formatPriceAndAvailability variantCombination =
    if variantCombination.available > 0 then
        if variantCombination.minPrice == variantCombination.maxPrice then
            (formatPrice variantCombination.minPrice)
        else
            (formatPrice variantCombination.minPrice) ++ " ~ " ++ (formatPrice variantCombination.maxPrice)
    else
        "Out of stock"


findVariant : List Variant -> String -> Maybe Variant
findVariant variants id =
    List.head (List.filter (\item -> item.id == id) variants)


viewItemSection : Model -> VariantType -> Int -> Html Msg
viewItemSection model section index =
    let
        checked =
            Array.get index (Array.fromList model.variantSelected)
    in
        div
            [ class "variant"
            ]
            [ h3 [] [ section.attribute |> text ]
            , div []
                (List.map
                    (\item -> viewItem model (checked == Just item) section.attribute item index)
                    (section.variantValues)
                )
            ]


viewJsonForm : Model -> Html Msg
viewJsonForm model =
    div []
        [ button [ onClick (DecodePayload 0) ] [ text "Apply this payload" ]
        , textarea [ onInput UpdatePayload ] [ text model.payloadJson ]
        ]


getPayload : Maybe Payload -> Payload
getPayload payload =
    case payload of
        Just data ->
            data

        Nothing ->
            emptyPayload


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

                variantCombination =
                    case Dict.get presentSelection model.variants of
                        Just data ->
                            data

                        Nothing ->
                            emptyVariantCombination

                variationsQuantity =
                    List.length variantCombination.ids

                stock =
                    variantCombination.available

                stillMoreThanOneVariant =
                    (List.length variantCombination.ids) > 1
            in
                div
                    [ class "main" ]
                    [ node "style" [] [ text css ]
                    , div [ class "header" ]
                        [ h1 []
                            [ variantCombination |> formatPriceAndAvailability |> (++) "Price " |> text
                            ]
                        , p []
                            ([ text "Variants: "
                             , text (variationsQuantity |> toString)
                             , text ", Stock: "
                             , text (stock |> toString)
                             , text ", Selected: "
                             ]
                                ++ (if variationsQuantity == 1 then
                                        case List.head variantCombination.ids of
                                            Just id ->
                                                let
                                                    payload =
                                                        getPayload model.payload

                                                    variantValues =
                                                        case findVariant payload.variants id of
                                                            Just data ->
                                                                data.variantValues

                                                            Nothing ->
                                                                emptyVariant.variantValues
                                                in
                                                    (List.map (\item -> span [ class "selected" ] [ text item ]) variantValues)

                                            Nothing ->
                                                [ text "" ]
                                    else
                                        let
                                            filterVariantSelected =
                                                List.filter (\item -> item /= "❌") model.variantSelected
                                        in
                                            (List.map (\item -> span [ class "selected" ] [ text item ]) filterVariantSelected)
                                   )
                            )
                        , button
                            [ classList
                                [ "cta" => True
                                , "disabled" => stillMoreThanOneVariant
                                ]
                            , disabled stillMoreThanOneVariant
                            ]
                            [ if stillMoreThanOneVariant then
                                text "Select a Product"
                              else
                                text "Add to Cart"
                            ]
                        ]
                    , div
                        [ class "body"
                        ]
                        (List.indexedMap (\index item -> (viewItemSection model item index)) payload.variantsTypes)
                    , div [ class "footer" ]
                        [ case model.error of
                            Nothing ->
                                text ""

                            Just data ->
                                div [ class "error" ]
                                    [ h4 [] [ "Error" |> text ]
                                    , p [] [ text data ]
                                    ]
                        , ul []
                            [ li [] [ a [ href "https://github.com/lucamug/elm-faceted-variations" ] [ text "Source Code" ] ]
                            , li [] [ a [ href "https://medium.com/@l.mugnaini/faceted-variants-in-elm-c38b4d661355" ] [ text "Post" ] ]
                            , li [] [ a [ href "https://lucamug.github.io/elm-faceted-variations/" ] [ text "Demo" ] ]
                            , li [] [ a [ href "https://lucamug.github.io/elm-faceted-variations/combinationGenerator.html" ] [ text "Combination Generator" ] ]
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
a {
    text-decoration: none;
    color: #ff73cc;
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
    padding-top: 120px;
    display: flex;
    flex-direction: row;
}
@media screen and (max-width: 700px) {
    .body {
        flex-direction: column;
    }
}
.footer {
    background-color: #eee;
    color: #aaa;
}
.variant {
    flex: 1;
    padding: 10px;
}
.viewItem {
    background-color: white;
    padding: 6px;
    margin: 6px 0;
    cursor: pointer;
    border: 3px solid white;
}
.selected {
    background-color: #ff73cc;
    color: white;
    padding: 3px 6px;
    margin: 0 2px;
}
.cta {
    background-color: #ff73cc;
    color: white;
    position: absolute;
    top: 20px;
    right: 40px;
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
    background-color: #ff73cc;
    color: white;
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
  },
  {
   "attribute": "Material",
   "variantValues": [
    "Metal",
    "Wood",
    "Glass",
    "Plastic"
   ]
  }
 ],
 "variants": [
  {
   "id": "0",
   "variantValues": [
    "Red",
    "Small",
    "Metal"
   ],
   "price": 1.9,
   "available": 1
  },
  {
   "id": "3",
   "variantValues": [
    "Red",
    "Small",
    "Plastic"
   ],
   "price": 4.9,
   "available": 4
  },
  {
   "id": "5",
   "variantValues": [
    "Red",
    "Medium",
    "Wood"
   ],
   "price": 6.9,
   "available": 6
  },
  {
   "id": "8",
   "variantValues": [
    "Red",
    "Large",
    "Metal"
   ],
   "price": 9.9,
   "available": 9
  },
  {
   "id": "11",
   "variantValues": [
    "Red",
    "Large",
    "Plastic"
   ],
   "price": 2.9,
   "available": 2
  },
  {
   "id": "13",
   "variantValues": [
    "Green",
    "Small",
    "Wood"
   ],
   "price": 4.9,
   "available": 4
  },
  {
   "id": "16",
   "variantValues": [
    "Green",
    "Medium",
    "Metal"
   ],
   "price": 7.9,
   "available": 7
  },
  {
   "id": "19",
   "variantValues": [
    "Green",
    "Medium",
    "Plastic"
   ],
   "price": 10.9,
   "available": 10
  },
  {
   "id": "21",
   "variantValues": [
    "Green",
    "Large",
    "Wood"
   ],
   "price": 2.9,
   "available": 2
  }
 ]
}"""
