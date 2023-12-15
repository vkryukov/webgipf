module Ui exposing
    ( Field
    , Form
    , viewForm
    , viewPrimaryButton
    , viewSecondaryButton
    )

import Html exposing (Html, button, div, h2, input, label, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


fst : ( a, b ) -> a
fst ( x, _ ) =
    x


snd : ( a, b ) -> b
snd ( _, y ) =
    y


type alias Field msg =
    { label : String
    , fieldType : String
    , placeholder : String
    , value : String
    , onInput : String -> msg
    }


viewField : Field msg -> Html msg
viewField field =
    div [ class "mb-4" ]
        [ label [ class "block text-gray-700 text-sm font-bold mb-2" ] [ text field.label ]
        , input
            [ class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            , type_ field.fieldType
            , placeholder field.placeholder
            , value field.value
            , onInput field.onInput
            ]
            []
        ]


viewPrimaryButton : ( String, msg ) -> Html msg
viewPrimaryButton ( label, msg ) =
    button [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline", onClick msg ] [ text label ]


viewSecondaryButton : ( String, msg ) -> Html msg
viewSecondaryButton ( label, msg ) =
    button [ class "inline-block align-baseline font-bold text-sm text-blue-500 hover:text-blue-800", onClick msg ] [ text label ]


type alias Form msg =
    { title : String
    , fields : List (Field msg)
    , primaryAction : ( String, msg )
    , secondaryAction : ( String, msg )
    }


viewForm : Form msg -> Html msg
viewForm form =
    div [ class "p-4" ]
        [ h2 [ class "text-lg font-bold mb-4" ] [ text form.title ]
        , div [ class "w-full max-w-xs" ]
            (List.map viewField form.fields
                ++ [ div [ class "flex justify-between mt-4" ]
                        [ viewPrimaryButton form.primaryAction
                        , viewSecondaryButton form.secondaryAction
                        ]
                   ]
            )
        ]
