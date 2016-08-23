port module Component.Editor exposing ( Model, init, Msg, update, view
                                      , subscriptions )
import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (startsWith, fromChar, endsWith, slice)
import Json.Decode exposing (at, string)
import Json.Encode
import Markdown
import Keyboard

-- MODEL

type alias Model =
  { content        : String
  , selected       : Selection
  , domId          : String
  }

{-| Record mapping useful values of the JS Selection object.
-}
type alias Selection =
  { text  : String
  , start : Int
  , end   : Int
  }

-- Init value for Selection record
defaultSelection = Selection "" 0 0


{-| Initialise the editor with the `id` of the dom node the editor loads into.
-}
init : String -> (Model, Cmd Msg)
init id = ( Model "" defaultSelection id
       , Cmd.none
       )


-- UPDATE

{-| [checkSelection domId ] gets the JS document.Selection object if within the DOM node
identified by its id value [domId]. This Selection object is mapped to the
[Editor.Selection] and sent back as a subscription.
-}
port checkSelection : String -> Cmd msg

{-|[moveCursor (domId, position)] moves the cursor to the index [position]
inside the DOM node identified by [domId].
-}
port moveCursor : (String, Int) -> Cmd msg


type Msg = Edited String      -- ^Text in editor was edited
         | Blank              -- ^Dummy message, does nothing
         | Selected Selection -- ^Some text was selected
         | CheckSelection     -- ^Message to request checking for any selection
         | Bold               -- ^Markdown bold
         | Emphasis           -- ^Markdown Emphasis
         | Header             -- ^Markdown header


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Dummy
    Blank -> (model, Cmd.none)

    Edited new ->
      let formatted = Debug.log "Content" new
      in ( { model | content = new }
         , checkSelection "editor-area"
         )

    CheckSelection -> (model, checkSelection model.domId)

    Bold -> (makeBold model, Cmd.none)

    Emphasis -> (makeEmphasis model, Cmd.none)

    Selected sel ->
      ( { model | selected = Debug.log "SELECTED" sel }
      , Cmd.none
      )

    Header ->
      ( makeHeading model
      , moveCursor (model.domId, model.selected.start)
      )


-- SUBSCRIPTIONS

-- | Receive a mapped [Editor.Selection] type if there was any selection.
port getSelected : (Selection -> msg) -> Sub msg


{-| Things we want to be subscribed to:
o Keypresses which may change the cursor position in the editable area without
  adding new content.
o Receiving new selection information as the [Editor.Selection] type.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  let check key =
        if key >= 65 && key <= 90
        then Blank
        else CheckSelection
  in Sub.batch
    [ 
    getSelected Selected
    ]


-- VIEW

{-| The view is divided into two main parts, which split over the full page
width equally. These are [editorView] and [previewView]. The former presents
the main editor area and the latter presents the markdown-converted HTML
preview.
-}
view : Model -> Html Msg
view model =
  row_
    [ colMd_ 12 12 6 [ editorView model ]
    , colMd_ 12 12 6 [ previewView model ]
    ]

--| Render the editor
editorView : Model -> Html Msg
editorView model =
  let toolbar = viewToolbar model
  in div [ class "editor" ]
    [ div [ class "editor-top" ] [ toolbar ]
    , div [ class "editor-text"
          , contenteditable True
          , id "editor-area"
          , onMouseUp CheckSelection
          , on "input" (Json.Decode.map Edited innerHtmlDecoder)
          --, property "innerHTML" (Json.Encode.string model.content)
          ]
        []
    , br [] []
    ]

{-| Custom decoder to extract the target.innerHtml field of a JS event -}
innerHtmlDecoder = at ["target", "innerHTML"] string


{-|  Render the HTML live preview -}
previewView : Model -> Html Msg
previewView model =
  div [ class "preview" ]
    [ Markdown.toHtml [] model.content ]


{-| Render the editor toolbar -}
viewToolbar : Model -> Html Msg
viewToolbar model =
  ul [ class "editor-toolbar" ]
    [ makeToolButton "Bold" Bold
    , makeToolButton "Emphasis" Emphasis
    , makeToolButton "H1" Header
    ]

{- | Create an li button representing a tool on the toolbar with a text [name]
and an onclick event [action].
-}
makeToolButton : String -> Msg -> Html Msg
makeToolButton name action =
  li [ onClick action ] [ text name]



-- TEXT MANIPULATION FUNCTIONS

-- | Markdown selection formatters

makeBold = selectionWrap "**"

makeEmphasis = selectionWrap "_"


{-| Wrap the selected content in the editor with a string `wrap`.

The `selected` field in the `model` provides the selected substring which
will be wrapped, and the actual indices of the range of substring in the
entire string.
-}
selectionWrap : String -> Model -> Model
selectionWrap wrap model =
  let sel = model.selected
      newContent =
        toggleWrapRange wrap model.content sel.text sel.start sel.end
  in { model
       | content = newContent
       , selected = defaultSelection
     }


{-| Wrap a substring within a string with a `wrap` string.

    toggleWrapRange "*" "foo bar" "bar" 4 6 == "foo *bar*"
-}
toggleWrapRange : String    -- ^String to wrap around selected text
                -> String -- ^The entire string encompassing the selected
                -> String -- ^The selected substring
                -> Int    -- ^start index of selection
                -> Int    -- ^end index of selection
                -> String -- ^result
toggleWrapRange wrap str sel start end =
  if sel == ""
  then str
  else
    let strLen = String.length str
        beginSlice = slice 0 start str
        endSlice   = slice end strLen str
        wrapped = toggleWrap wrap sel
    in beginSlice ++ wrapped ++ endSlice


{-| Wrap a string [str] with [wrap] or unwrap if it already is wrapped. -}
toggleWrap : String -> String -> String
toggleWrap wrap str =
  if startsWith wrap str && endsWith wrap str
  then slice 1 -1 str
  else wrap ++ str ++ wrap


{-| Make the current line a markdown header. -}
makeHeading : Model -> Model
makeHeading ({content, selected} as model) =
  let lines = String.lines content
      transformed = withCursorLine selected.start (Debug.log "Lines" lines) ((++) "# ")
  in { model
       | content = String.join "\n" transformed
     }


{-| Move through the `lines` until the line at which the `cursor` is at is
reached and then apply the `transform` on that line only. -}
withCursorLine : Int -> List String -> (String -> String)
               -> List String
withCursorLine cursor lines transform =
  case lines of
    [] -> []
    (hd :: tl) ->
      let len = String.length hd + 1          
      in if len <= cursor
         then
           hd :: withCursorLine (cursor - len) tl transform
         else
           (transform hd) :: tl
