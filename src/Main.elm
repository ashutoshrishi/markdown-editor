import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

-- Components
import Component.Editor as Editor

main =
  App.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { editorModel : Editor.Model }


init : (Model, Cmd Msg)
init =
  let (editorInit, em) = Editor.init "editor-area"
  in ( Model editorInit
     , Cmd.map EditorMsg em 
     )


-- UPDATE

type Msg = EditorMsg Editor.Msg

  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditorMsg emsg ->
      let (newEditor, newMsg) = Editor.update emsg model.editorModel
      in ( { model | editorModel=newEditor }
         , Cmd.map EditorMsg newMsg
         )
                 
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map EditorMsg (Editor.subscriptions model.editorModel)
  
-- VIEW            

{-| View the Index page, with the content area changing according to the
current url page. -}
view : Model -> Html Msg
view model =
  containerFluid_    
    [ row_
        [ colMd_ 12 12 6
            [ div [ class "page-header" ]
                [ h1 [] [ text "Markdown Editor" ] ]
            ]
        ]
    , App.map EditorMsg (Editor.view model.editorModel)
    ]
