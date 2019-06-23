module Main exposing (main)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, paddingXY, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Time



{-
   INIT
-}


type alias Model =
    { compute : Float
    , crypto : Float
    , drones : List Drone
    , chosenComponents : List Component
    , nextDroneId : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        components =
            [ HealthComponent ]
    in
    ( { compute = 0
      , crypto = 50
      , drones = [ initDrone 0 components ]
      , chosenComponents = []
      , nextDroneId = 1
      }
    , Cmd.none
    )



{-
   DRONES
-}


type Component
    = HealthComponent
    | PerformanceComponent


allComponents : List Component
allComponents =
    let
        {- This exists only to cause compilation errors when a new value for Component is added -}
        validator =
            case HealthComponent of
                HealthComponent ->
                    ()

                PerformanceComponent ->
                    ()
    in
    [ HealthComponent, PerformanceComponent ]


type Drone
    = Drone Int Stats (List Component)


initDrone : Int -> List Component -> Drone
initDrone id components =
    Drone id (componentsToMaxStats components) components


type alias Stats =
    { health : Int, performance : Int }


baseStats : Stats
baseStats =
    { health = 10
    , performance = 5
    }


emptyStats : Stats
emptyStats =
    { health = 0
    , performance = 0
    }


combineStats : Stats -> Stats -> Stats
combineStats a b =
    { health = a.health + b.health
    , performance = a.performance + b.performance
    }


componentToMaxStats : Component -> Stats
componentToMaxStats component =
    case component of
        HealthComponent ->
            { health = 10, performance = 0 }

        PerformanceComponent ->
            { health = 0, performance = 1 }


componentsToMaxStats : List Component -> Stats
componentsToMaxStats =
    List.foldl (combineStats << componentToMaxStats) baseStats


droneToMaxStats : Drone -> Stats
droneToMaxStats drone =
    let
        components =
            case drone of
                Drone _ _ c ->
                    c
    in
    componentsToMaxStats components


componentToCost : Component -> Int
componentToCost component =
    case component of
        HealthComponent ->
            5

        PerformanceComponent ->
            5


componentsToCost : List Component -> Int
componentsToCost =
    List.foldl (\c cost -> componentToCost c + cost) 10


componentName : Component -> String
componentName component =
    case component of
        HealthComponent ->
            "Health"

        PerformanceComponent ->
            "Performance"


getHealth : Drone -> Int
getHealth (Drone _ stats _) =
    stats.health


getPerformance : Drone -> Int
getPerformance (Drone _ stats _) =
    stats.performance



{-
   UPDATE
-}


type Msg
    = CreateDrone (List Component)
    | SelectComponent Component
    | DeselectComponent Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateDrone components ->
            let
                newDrone =
                    initDrone model.nextDroneId model.chosenComponents

                cost =
                    componentsToCost model.chosenComponents
            in
            if model.crypto - toFloat cost < 0 then
                ( model, Cmd.none )

            else
                ( { model
                    | drones = newDrone :: model.drones
                    , chosenComponents = []
                    , crypto = model.crypto - toFloat cost
                    , nextDroneId = model.nextDroneId + 1
                  }
                , Cmd.none
                )

        SelectComponent component ->
            ( { model | chosenComponents = component :: model.chosenComponents }, Cmd.none )

        DeselectComponent index ->
            ( { model | chosenComponents = List.take index model.chosenComponents ++ List.drop (index + 1) model.chosenComponents }, Cmd.none )

        Tick _ ->
            let
                { cryptoDelta, computeDelta } =
                    tick model.drones
            in
            ( { model | crypto = model.crypto + cryptoDelta, compute = model.compute + computeDelta }, Cmd.none )


tick : List Drone -> { cryptoDelta : Float, computeDelta : Float }
tick drones =
    let
        speedFactor =
            0.001

        stats =
            drones
                |> List.map droneToMaxStats
                |> List.foldl combineStats emptyStats
    in
    { cryptoDelta = 0 * speedFactor, computeDelta = toFloat stats.performance * speedFactor }



{-
   SUBSCRIPTIONS
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



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
    | Grey


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

        Grey ->
            Element.rgb255 155 155 155


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
        droneId =
            case drone of
                Drone id _ _ ->
                    id
    in
    column [ padding Medium, backgroundColor MidnightGreen, rounded, fontColor White, width (Element.maximum 300 fill) ]
        [ text ("Drone " ++ String.fromInt droneId)
        , row []
            [ text (componentName HealthComponent ++ ": ")
            , text <| String.fromInt (getHealth drone)
            ]
        , row
            []
            [ text (componentName PerformanceComponent ++ ": ")
            , text <| String.fromInt (getPerformance drone)
            ]
        ]


renderComponentButton : Component -> Msg -> Element Msg
renderComponentButton component msg =
    button [] { onPress = Just msg, label = text (componentName component ++ " " ++ String.fromInt (componentToCost component)) }


label : List (Element.Attribute msg) -> String -> Element msg
label attrs message =
    el ([ Font.size 12 ] ++ attrs) (text (String.toUpper message))


renderCreateDronePanel : Float -> List Component -> Element Msg
renderCreateDronePanel crypto chosenComponents =
    column [ padding Medium, backgroundColor White, rounded, spacing Medium, width (px 500) ]
        [ column
            [ Border.solid
            , borderColor Timberwolf
            , Border.width 1
            , padding Small
            , rounded
            , spacing Small
            , width fill
            ]
            [ label [] "Store"
            , Element.wrappedRow
                [ spacing ExtraSmall
                , width fill
                ]
                (List.map (\c -> renderComponentButton c (SelectComponent c)) allComponents)
            ]
        , label [] "loadout"
        , if List.length chosenComponents == 0 then
            label [ Font.bold ] "Choose components from the store above"

          else
            Element.wrappedRow [ spacing ExtraSmall ] (text "Base cost 10 + " :: List.indexedMap (\i c -> renderComponentButton c (DeselectComponent i)) chosenComponents)
        , let
            canAfford =
                crypto - toFloat (componentsToCost chosenComponents) >= 0
          in
          button
            ((if canAfford then
                []

              else
                [ backgroundColor Grey ]
             )
                ++ [ alignRight ]
            )
            { onPress =
                if canAfford then
                    Just (CreateDrone chosenComponents)

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
        (column [ centerX, paddingXY None Large, spacing Medium ]
            [ Element.wrappedRow [ spacing Small, width (px 800) ] (List.map renderDrone model.drones)
            , text <| "Crypto: " ++ String.fromFloat model.crypto
            , text <| "Compute: " ++ String.fromFloat model.compute
            , renderCreateDronePanel model.crypto model.chosenComponents
            ]
        )



{-
   PROGRAM
-}


main =
    Browser.element
        { init = init, view = view, update = update, subscriptions = subscriptions }
