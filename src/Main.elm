module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font exposing (center)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List exposing (sum)
import List.Extra exposing (getAt)


main =
    Browser.document { init = init, subscriptions = subscriptions, update = update, view = view }


type Msg
    = Next
    | Previous
    | ToggleA
    | ToggleB
    | WindowResize Int Int
    | ToggleAbout


type alias Plant =
    { name : String, file : String }


type alias PlantPair =
    { selection : Maybe Selection
    , a : Plant
    , b : Plant
    }


plantPairs : List PlantPair
plantPairs =
    [ { selection = Nothing
      , a = { name = "Goldrute", file = "Goldrute.jpg" }
      , b = { name = "echtes Johanniskraut", file = "echtes-Johanniskraut.jpg" }
      }
    , { selection = Nothing
      , a = { name = "echte Kamille", file = "echte-Kamille.jpg" }
      , b = { name = "einjähriges Berufskraut", file = "einjaehriges-Berufskraut.jpg" }
      }
    , { selection = Nothing
      , a = { name = "Kirschlorbeer", file = "kirschlorbeer.jpg" }
      , b = { name = "gemeiner Liguster", file = "gemeiner-Liguster.jpg" }
      }
    , { selection = Nothing
      , a = { name = "schmalblättriges Greiskraut", file = "schmalblaettriges-Greiskraut.jpg" }
      , b = { name = "Jakobs-Kreuzkraut", file = "jakobs-kreuzkraut.jpg" }
      }
    , { selection = Nothing
      , a = { name = "schlaffe Segge", file = "schlaffe-Segge.jpg" }
      , b = { name = "Erdmandelgras", file = "erdmandelgras.jpg" }
      }
    , { selection = Nothing
      , a = { name = "Wiesenbärenklau", file = "Wiesen-Baerenklau.jpg" }
      , b = { name = "Riesenbärenklau", file = "Riesenbaerenklau.jpg" }
      }
    ]


type alias Model =
    { index : Int
    , plantPairs : List PlantPair
    , windowWidth : Int
    , windowHeight : Int
    , showAbout : Bool
    }


type alias InitFlags =
    { windowWidth : Int
    , windowHeight : Int
    }


type Selection
    = SelectA
    | SelectB


init : InitFlags -> ( Model, Cmd Msg )
init flags =
    ( { index = 0
      , plantPairs = plantPairs
      , windowWidth = flags.windowWidth
      , windowHeight = flags.windowHeight
      , showAbout = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize WindowResize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleSelected : Selection -> Maybe Selection -> Maybe Selection
        toggleSelected chosen selection =
            case selection of
                Just SelectA ->
                    if chosen == SelectA then
                        Nothing

                    else
                        Just SelectB

                Just SelectB ->
                    if chosen == SelectB then
                        Nothing

                    else
                        Just SelectA

                Nothing ->
                    Just chosen

        toggle : Selection -> List PlantPair
        toggle chosen =
            model.plantPairs
                |> List.indexedMap
                    (\i e ->
                        if i == model.index then
                            { e
                                | selection = toggleSelected chosen e.selection
                            }

                        else
                            e
                    )
    in
    case msg of
        WindowResize w h ->
            ( { model | windowWidth = w, windowHeight = h }, Cmd.none )

        ToggleAbout ->
            ( { model | showAbout = not model.showAbout }, Cmd.none )

        Next ->
            ( { model | index = model.index + 1 }, Cmd.none )

        Previous ->
            ( { model | index = model.index - 1 }, Cmd.none )

        ToggleA ->
            ( { model | plantPairs = toggle SelectA }, Cmd.none )

        ToggleB ->
            ( { model | plantPairs = toggle SelectB }, Cmd.none )


allChosen : Model -> Bool
allChosen model =
    countChosen model == List.length model.plantPairs


countChosen : Model -> Int
countChosen model =
    let
        sumSelected : PlantPair -> Int -> Int
        sumSelected entry acc =
            case entry.selection of
                Just _ ->
                    acc + 1

                Nothing ->
                    acc
    in
    model.plantPairs
        |> List.foldl sumSelected 0


buildAnswer : Model -> String
buildAnswer model =
    let
        linebreak =
            "%0D%0A"

        sumSelected : PlantPair -> String -> String
        sumSelected entry acc =
            case entry.selection of
                Just SelectA ->
                    acc ++ linebreak ++ entry.a.name

                Just SelectB ->
                    acc ++ linebreak ++ entry.b.name

                Nothing ->
                    acc
    in
    model.plantPairs
        |> List.foldl sumSelected ("Meine Lösung lautet:" ++ linebreak)


view : Model -> Browser.Document Msg
view model =
    { title = "Neophyten Quiz"
    , body = [ viewApp model ]
    }


viewInfo : Model -> Element Msg
viewInfo model =
    if model.showAbout then
        el
            [ height shrink
            , width shrink
            , Border.solid
            , Border.width 2
            , Border.color fontColor
            , padding 10
            , Background.color screenBackgroundColor
            , centerX
            , centerY
            , Font.color fontColor
            , onClick ToggleAbout
            ]
            (textColumn
                [ spacing 5
                , width (fill |> maximum (model.windowWidth - 100))
                , height (fill |> maximum (model.windowHeight // 2))
                , scrollbars
                ]
                [ paragraph [ Font.center, Font.size 30 ] [ text "Welches ist der Neophyt?" ]
                , paragraph [] [ text "Auf den folgenden Bildpaaren muss jeweils der Neophyt durch clicken ausgewählt werden." ]
                , paragraph []
                    [ text "Wenn auf allen Bildpaaren eines ausgewählt ist, kann die Auswahl mit " ]
                , paragraph []
                    [ el [ Font.bold, Border.rounded 4, Border.width 1, padding 3 ] (text "Lösung abschicken") ]
                , paragraph [] [ text " gemailt werden." ]
                ]
            )

    else
        none


viewApp : Model -> Html Msg
viewApp model =
    let
        alreadyChosen =
            countChosen model

        selectionScore : String
        selectionScore =
            if alreadyChosen == 0 then
                "Hilfe"

            else
                String.fromInt (countChosen model) ++ "/" ++ String.fromInt (List.length model.plantPairs)
    in
    Element.layout [ inFront (viewInfo model) ]
        (column
            [ spacing 5, width fill, height fill, Font.color fontColor, Background.color screenBackgroundColor ]
            [ row [ width fill ]
                [ el [ centerX, Font.center, Font.size 30 ] (text "Neophyten Quiz")
                , el (buttonStyle True ++ [ Font.size 30, alignRight, padding 10, onClick ToggleAbout ]) (text selectionScore)
                ]
            , el
                [ width fill, height (fill |> maximum (model.windowHeight - 200)) ]
                (model.plantPairs
                    |> getAt model.index
                    |> Maybe.map (viewPlantPair model)
                    |> Maybe.withDefault none
                )
            , row [ width fill, spacing 10 ]
                [ viewButton
                    (if model.index > 0 then
                        Just Previous

                     else
                        Nothing
                    )
                    "<<"
                , viewButton
                    (if (model.index + 1) < List.length model.plantPairs then
                        Just Next

                     else
                        Nothing
                    )
                    ">>"
                , if allChosen model then
                    link
                        (buttonStyle True
                            ++ [ Font.size 30 ]
                        )
                        { url = "mailto:irene.troxler@datazug.ch?subject=Neophyten-Quiz&body=" ++ buildAnswer model
                        , label = text "Lösung\nabschicken"
                        }

                  else
                    none
                ]
            ]
        )


viewButton : Maybe Msg -> String -> Element Msg
viewButton msg title =
    let
        enabled =
            case msg of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Input.button (buttonStyle enabled)
        { onPress = msg
        , label = el [] (text title)
        }


viewPlantPair : Model -> PlantPair -> Element Msg
viewPlantPair model neophyte =
    let
        border sel =
            if isSelected sel neophyte.selection then
                [ Border.glow white 5 ]

            else
                []

        responsive =
            if model.windowWidth < model.windowHeight then
                -- vertical
                { layout = column
                , scaleAttrs = [ width (fill |> maximum model.windowWidth), height (fill |> maximum ((model.windowHeight - 150) // 2)) ]
                }

            else
                -- horizontal
                { layout = row
                , scaleAttrs = [ width (fill |> maximum model.windowWidth), height (shrink |> maximum (model.windowHeight - 100)) ]
                }
    in
    responsive.layout [ width fill, height fill, spacing 5 ]
        [ column
            [ width fill, height fill ]
            [ el [ centerX, Background.color screenBackgroundColor ] (text neophyte.a.name)
            , el
                (border SelectA
                    ++ [ width fill, height fill, onClick ToggleA ]
                )
                (image responsive.scaleAttrs { src = neophyte.a.file, description = neophyte.a.name })
            ]
        , column [ width fill, height fill ]
            [ el [ centerX, Background.color screenBackgroundColor ] (text neophyte.b.name)
            , el
                (border SelectB
                    ++ [ width fill, height fill, onClick ToggleB ]
                )
                (image responsive.scaleAttrs { src = neophyte.b.file, description = neophyte.b.name })
            ]
        ]


isSelected : Selection -> Maybe Selection -> Bool
isSelected choose selection =
    selection
        |> Maybe.map
            (\v -> v == choose)
        |> Maybe.withDefault False


buttonStyle enabled =
    let
        colors =
            if enabled then
                [ Font.color buttonFontColor, Background.color buttonBackgroundColor ]

            else
                [ Font.color screenBackgroundColor ]
    in
    [ centerX, padding 15, Border.rounded 3, Font.size 50 ] ++ colors


screenBackgroundColor =
    rgb255 0 100 0


fontColor =
    rgb255 240 240 240


buttonFontColor =
    rgb255 60 60 60


buttonBackgroundColor =
    rgb255 138 138 138


white =
    rgb255 255 255 255
