module Ui exposing
    ( Field
    , Form
    , viewBoldText
    , viewErrorMessage
    , viewForm
    , viewNavBar
    , viewPrimaryButton
    , viewSecondaryButton
    , viewSiteTitle
    , viewText
    )

import Html exposing (Html, a, button, div, h2, input, label, nav, span, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Field msg =
    { label : String
    , fieldType : String
    , placeholder : String
    , value : String
    , onInput : String -> msg
    , highlight : Bool
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
            , classList [ ( "border-red-500", field.highlight ) ]
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
    , error : Maybe String
    }


viewErrorMessage : Maybe String -> Html msg
viewErrorMessage maybeError =
    case maybeError of
        Just errorMsg ->
            div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative mb-4 max-w-s" ]
                [ span [ class "block sm:inline" ] [ text errorMsg ] ]

        Nothing ->
            div [] []


viewForm : Form msg -> Html msg
viewForm form =
    div [ class "p-4" ]
        [ h2 [ class "text-lg font-bold mb-4" ] [ text form.title ]
        , div [ class "w-full max-w-xs" ]
            (viewErrorMessage form.error
                :: (List.map viewField form.fields
                        ++ [ div [ class "flex justify-between mt-4" ]
                                [ viewPrimaryButton form.primaryAction
                                , viewSecondaryButton form.secondaryAction
                                ]
                           ]
                   )
            )
        ]


viewText : String -> Html msg
viewText text_ =
    div [ class "mr-4" ] [ text text_ ]


viewBoldText : String -> Html msg
viewBoldText text_ =
    div [ class "font-bold mr-4" ] [ text text_ ]


viewSiteTitle : String -> Html msg
viewSiteTitle text_ =
    div [ class "font-sitename font-bold mr-4" ] [ text text_ ]


viewNavBar : List (Html msg) -> List ( String, msg ) -> Html msg
viewNavBar items actions =
    nav [ class "w-full flex justify-between items-center bg-gray-200 p-4" ]
        [ div [ class "flex" ] items
        , div [ class "flex" ]
            (List.map
                (\( label, actionMsg ) ->
                    a [ class "text-blue-500 hover:underline mr-4 cursor-pointer", onClick actionMsg ] [ text label ]
                )
                actions
            )
        ]
