module Ui exposing
    ( Field
    , Form
    , viewBoldText
    , viewErrorMessage
    , viewForm
    , viewH2
    , viewNavBar
    , viewPrimaryButton
    , viewRadio
    , viewSecondaryButton
    , viewSection
    , viewSiteTitle
    , viewTable
    , viewText
    )

import Html exposing (Html, a, button, div, h2, input, label, nav, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, name, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)


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
    viewSection form.title
        [ div [ class "w-full max-w-xs" ]
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


viewSection : String -> List (Html msg) -> Html msg
viewSection title items =
    div [ class "mb-4 p-4" ]
        [ viewH2 title
        , div [ class "flex flex-col" ] items
        ]


viewH2 : String -> Html msg
viewH2 text_ =
    h2 [ class "text-lg font-bold mb-4" ] [ text text_ ]


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


viewTable : List a -> List String -> List (a -> String) -> Html msg
viewTable objs headers fields =
    div [ class "flex flex-col" ]
        [ div [ class "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" ]
            [ div [ class "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" ]
                [ div [ class "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" ]
                    [ table [ class "divide-y divide-gray-200" ]
                        [ thead []
                            [ tr []
                                (List.map
                                    (\header ->
                                        th [ class "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" ] [ text header ]
                                    )
                                    headers
                                )
                            ]
                        , tbody [ class "bg-white divide-y divide-gray-200" ]
                            (List.map
                                (\obj ->
                                    tr []
                                        (List.map
                                            (\field ->
                                                td [ class "px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500" ] [ text (field obj) ]
                                            )
                                            fields
                                        )
                                )
                                objs
                            )
                        ]
                    ]
                ]
            ]
        ]


viewCheckBox : Bool -> String -> (Bool -> msg) -> Html msg
viewCheckBox isChecked lbl onCheck =
    div [ class "flex items-center" ]
        [ input [ type_ "checkbox", checked isChecked, onClick (onCheck (not isChecked)) ] []
        , label [ class "ml-2 block text-sm leading-5 text-gray-700" ] [ text lbl ]
        ]


viewRadio : Bool -> String -> msg -> msg -> Html msg
viewRadio checked_ lbl checkMsg noOpMsg =
    div [ class "flex items-center" ]
        [ input
            [ type_ "radio"
            , class "form-radio"
            , checked checked_
            , onCheck
                (\isChecked ->
                    if isChecked then
                        checkMsg

                    else
                        noOpMsg
                )
            ]
            []
        , label [ class "ml-2 block text-sm leading-5 text-gray-700" ] [ text lbl ]
        ]
