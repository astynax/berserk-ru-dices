module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Collage as C exposing (defaultLineStyle)
import Collage.Events as Events
import Collage.Layout as Layout
import Collage.Render exposing (svgBox)
import Collage.Text as Text
import Color
import Glyph
import Html exposing (Html)
import Html.Attributes as HA
import Random
import Task


type alias Model =
    { throw : Maybe Throw
    , width : Float
    , height : Float
    }


type Msg
    = Set (Maybe Throw)
    | Resize Float Float
    | ThrowOne
    | ThrowTwo


type Strike
    = WeakStrike
    | MediumStrike
    | StrongStrike


type Throw
    = Strike Strike
    | CounterStrike
    | Miss


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions =
            always <|
                Browser.Events.onResize
                    (\w h -> Resize (toFloat w) (toFloat h))
        }


init () =
    ( { throw = Nothing
      , width = 200
      , height = 300
      }
    , Task.perform (\vp -> Resize vp.viewport.width vp.viewport.height)
        Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model
                | width = w
                , height = h - 10
              }
            , Cmd.none
            )

        Set value ->
            ( { model | throw = value }
            , Cmd.none
            )

        ThrowOne ->
            ( model, Random.generate (Set << Just) strikeThrow )

        ThrowTwo ->
            ( model, Random.generate (Set << Just) throw )


view : Model -> Browser.Document Msg
view model =
    { title = "Кубики для Берсерка"
    , body = [ scene model.width model.height model.throw ]
    }


scene w h t =
    let
        s =
            min w (h - 80 - 30)
    in
    svgBox ( w, h ) <|
        Layout.center <|
            Layout.vertical
                [ C.html ( w, 80 ) [] <|
                    Html.h3 []
                        [ Html.text "Кубики для"
                        , Html.br [] []
                        , Html.text " Берсерка"
                        ]
                , Maybe.withDefault (menu s) <|
                    Maybe.map (Events.onClick (Set Nothing) << result s)
                        t
                , C.html ( w, 30 ) [] <|
                    Html.a
                        [ HA.href Glyph.sourceUrl
                        , HA.style "font-size" "x-small"
                        ]
                        [ Html.text "Изображения взяты здесь" ]
                ]


card w h =
    C.styled
        ( C.uniform <| Color.rgb255 255 240 200
        , { defaultLineStyle
            | thickness = C.thick
            , fill = C.uniform Color.black
          }
        )
    <|
        C.roundedRectangle w h 15


label s =
    C.rendered
        << Text.size (round <| s / 10)
        << Text.shape Text.Upright
        << Text.fromString


glyph s =
    C.scale (s / 4 / 60) << C.image ( 60, 60 )


glyphBW s =
    glyph s Glyph.blackCounterStrike


glyphWW s =
    glyph s Glyph.whiteWeak


menu s =
    Layout.vertical
        [ Events.onClick ThrowTwo <|
            menuItem s <|
                Layout.horizontal
                    [ glyphBW s, Layout.spacer (s / 8) 0, glyphWW s ]
        , Events.onClick ThrowOne <|
            menuItem s (glyphWW s)
        ]


menuItem s icons =
    Layout.impose (Layout.center icons) <| card s (s / 2)


resultCard s img text =
    Layout.impose
        (Layout.vertical
            [ glyph (s * 2) img
            , Layout.spacer 0 20
            , label s text
            ]
        )
        (card s s)


result s x =
    let
        ( g, t ) =
            case x of
                Miss ->
                    ( Glyph.blackMiss, "Промах" )

                CounterStrike ->
                    ( Glyph.blackCounterStrike, "Слабый ответ" )

                Strike WeakStrike ->
                    ( Glyph.whiteWeak, "Слабый удар" )

                Strike MediumStrike ->
                    ( Glyph.whiteMedium, "Средний удар" )

                Strike StrongStrike ->
                    ( Glyph.whiteStrong, "Сильный удар" )
    in
    resultCard s g t



-- generation


strikeThrow =
    Random.map Strike <|
        Random.uniform StrongStrike
            [ MediumStrike
            , MediumStrike
            , WeakStrike
            , WeakStrike
            , WeakStrike
            ]


throw =
    strikeThrow
        |> Random.andThen
            (\s -> Random.uniform CounterStrike [ Miss, s, s, s, s ])
