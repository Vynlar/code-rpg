module Main exposing (main)

import Browser
import Element exposing (Element, centerX, centerY, column, el, fill, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



{-
   INIT
-}


type alias Model =
    { compute : Int
    , crypto : Int
    , drones : List Drone
    , chosenComponents : List Component
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { compute = 0
      , crypto = 50
      , drones = [ Drone [ Health ] ]
      , chosenComponents = []
      }
    , Cmd.none
    )



{-
   DRONES
-}


type Component
    = Health
    | Capacity


allComponents : List Component
allComponents =
    let
        {- This exists only to cause compilation errors when a new value for Component is added -}
        validator =
            case Health of
                Health ->
                    1

                Capacity ->
                    1
    in
    [ Health, Capacity ]


type Drone
    = Drone (List Component)


type alias Stats =
    { health : Int, capacity : Int }


baseStats : Stats
baseStats =
    { health = 10
    , capacity = 5
    }


combineStats : Stats -> Stats -> Stats
combineStats a b =
    { health = a.health + b.health
    , capacity = a.capacity + b.capacity
    }


componentsToStats : Component -> Stats
componentsToStats component =
    case component of
        Health ->
            { health = 10, capacity = 0 }

        Capacity ->
            { health = 0, capacity = 3 }


droneToStats : Drone -> Stats
droneToStats drone =
    let
        components =
            case drone of
                Drone c ->
                    c
    in
    List.foldl (combineStats << componentsToStats) baseStats components


componentToCost : Component -> Int
componentToCost component =
    case component of
        Health ->
            5

        Capacity ->
            5


componentsToCost : List Component -> Int
componentsToCost =
    List.foldl (\c cost -> componentToCost c + cost) 10


componentName : Component -> String
componentName component =
    case component of
        Health ->
            "Health"

        Capacity ->
            "Capacity"


componentValue : Drone -> Component -> Int
componentValue drone component =
    let
        stats =
            droneToStats drone
    in
    case component of
        Health ->
            stats.health

        Capacity ->
            stats.capacity


getHealth : Drone -> Int
getHealth drone =
    componentValue drone Health


getCapacity : Drone -> Int
getCapacity drone =
    componentValue drone Capacity



{-
   UPDATE
-}


type Msg
    = CreateDrone Drone
    | SelectComponent Component
    | DeselectComponent Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateDrone drone ->
            let
                newDrone =
                    Drone model.chosenComponents

                cost =
                    componentsToCost model.chosenComponents
            in
            if model.crypto - cost < 0 then
                ( model, Cmd.none )

            else
                ( { model | drones = newDrone :: model.drones, chosenComponents = [], crypto = model.crypto - cost }, Cmd.none )

        SelectComponent component ->
            ( { model | chosenComponents = component :: model.chosenComponents }, Cmd.none )

        DeselectComponent index ->
            ( { model | chosenComponents = List.take index model.chosenComponents ++ List.drop (index + 1) model.chosenComponents }, Cmd.none )



{-
   SUBSCRIPTIONS
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   VIEW
-}
{- Colors -}


type Color
    = BlueGreen
    | Seaweed
    | MidnightGreen
    | Black
    | Timberwolf
    | White


colorToElementColor : Color -> Element.Color
colorToElementColor color =
    case color of
        BlueGreen ->
            Element.rgb255 16 155 163

        Seaweed ->
            Element.rgb255 12 116 137

        MidnightGreen ->
            Element.rgb255 18 80 91

        Black ->
            Element.rgb255 5 4 4

        Timberwolf ->
            Element.rgb255 214 216 205

        White ->
            Element.rgb255 255 255 255


backgroundColor : Color -> Element.Attr decorative msg
backgroundColor color =
    let
        elementColor =
            colorToElementColor color
    in
    Background.color elementColor


fontColor : Color -> Element.Attr decorative msg
fontColor color =
    let
        elementColor =
            colorToElementColor color
    in
    Font.color elementColor


borderColor : Color -> Element.Attr decorative msg
borderColor color =
    let
        elementColor =
            colorToElementColor color
    in
    Border.color elementColor



{- Styles -}


rounded : Element.Attribute msg
rounded =
    Border.rounded 5


type Spacing
    = None
    | ExtraSmall
    | Small
    | Medium
    | Large
    | ExtraLarge


scaled : Int -> Float
scaled =
    Element.modular 4 2


spacingToInt : Spacing -> Int
spacingToInt s =
    case s of
        None ->
            0

        ExtraSmall ->
            round (scaled 1)

        Small ->
            round (scaled 2)

        Medium ->
            round (scaled 3)

        Large ->
            round (scaled 4)

        ExtraLarge ->
            round (scaled 5)


spacing : Spacing -> Element.Attribute msg
spacing s =
    Element.spacing (spacingToInt s)


padding : Spacing -> Element.Attribute msg
padding s =
    Element.padding (spacingToInt s)


paddingXY : Spacing -> Spacing -> Element.Attribute msg
paddingXY x y =
    Element.paddingXY (spacingToInt x) (spacingToInt y)



{- Components -}


renderDrone : Drone -> Element Msg
renderDrone drone =
    let
        kind =
            case drone of
                Drone _ ->
                    "Basic"
    in
    column [ padding Medium, backgroundColor MidnightGreen, rounded, fontColor White ]
        [ text kind
        , row []
            [ text (componentName Health ++ ": ")
            , text <| String.fromInt (getHealth drone)
            ]
        , row
            []
            [ text (componentName Capacity ++ ": ")
            , text <| String.fromInt (getCapacity drone)
            ]
        ]


renderComponentButton : Component -> Msg -> Element Msg
renderComponentButton component msg =
    button [] { onPress = Just msg, label = text (componentName component ++ " " ++ String.fromInt (componentToCost component)) }


label : List (Element.Attribute msg) -> String -> Element msg
label attrs message =
    el ([ Font.size 12 ] ++ attrs) (text (String.toUpper message))


renderCreateDronePanel : Int -> List Component -> Element Msg
renderCreateDronePanel crypto chosenComponents =
    column [ padding Medium, backgroundColor White, rounded, spacing Medium, width (px 500) ]
        [ label [] "Store"
        , row
            [ spacing ExtraSmall
            , padding Small
            , Border.solid
            , borderColor Timberwolf
            , Border.width 1
            , rounded
            , width fill
            ]
            (List.map (\c -> renderComponentButton c (SelectComponent c)) allComponents)
        , label [] "loadout"
        , if List.length chosenComponents == 0 then
            label [ Font.bold ] "Choose components from the store above"

          else
            row [ spacing ExtraSmall ] (text "Base cost 10 + " :: List.indexedMap (\i c -> renderComponentButton c (DeselectComponent i)) chosenComponents)
        , button []
            { onPress =
                if crypto - componentsToCost chosenComponents >= 0 then
                    Just (CreateDrone (Drone []))

                else
                    Nothing
            , label = text ("Create for " ++ String.fromInt (componentsToCost chosenComponents))
            }
        ]


button : List (Element.Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attributes =
    Input.button
        ([ backgroundColor Seaweed
         , fontColor White
         , paddingXY Medium Small
         , rounded
         ]
            ++ attributes
        )


view : Model -> Html Msg
view model =
    Element.layout [ backgroundColor Timberwolf ]
        (column [ centerX, paddingXY None Large, spacing Medium ] <|
            List.map renderDrone model.drones
                ++ [ text <| "Crypto: " ++ String.fromInt model.crypto ]
                ++ [ renderCreateDronePanel model.crypto model.chosenComponents ]
        )



{-
   PROGRAM
-}


main =
    Browser.element
        { init = init, view = view, update = update, subscriptions = subscriptions }
