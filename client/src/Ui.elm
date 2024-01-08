module Ui exposing
    ( Field
    , Form
    , Position(..)
    , ToolbarButton
    , viewBoldLink
    , viewBoldText
    , viewBreadcrumbs
    , viewErrorMessage
    , viewForm
    , viewH2
    , viewHtmlTable
    , viewLink
    , viewNavBar
    , viewPrimaryButton
    , viewRadio
    , viewSecondaryButton
    , viewSection
    , viewSiteTitle
    , viewSmallPrimaryButton
    , viewSmallSecondaryButton
    , viewStringTable
    , viewText
    , viewToolbar
    )

import Html exposing (Html, a, button, div, h2, input, label, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Routes


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


viewSmallPrimaryButton : ( String, msg ) -> Html msg
viewSmallPrimaryButton ( label, msg ) =
    button [ class "bg-blue-500 hover:bg-blue-700 text-white text-xs font-bold py-1 px-2 rounded focus:outline-none focus:shadow-outline", onClick msg ] [ text label ]


viewSecondaryButton : ( String, msg ) -> Html msg
viewSecondaryButton ( label, msg ) =
    button [ class "inline-block align-baseline font-bold text-sm text-blue-500 hover:text-blue-800", onClick msg ] [ text label ]


viewSmallSecondaryButton : ( String, msg ) -> Html msg
viewSmallSecondaryButton ( label, msg ) =
    button [ class "inline-block align-baseline font-bold text-xs text-blue-500 hover:text-blue-800", onClick msg ] [ text label ]


type alias Form msg =
    { title : String
    , fields : List (Field msg)
    , actions : List (Html msg)
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
                                form.actions
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
    div [ class "font-bold" ] [ text text_ ]


viewSiteTitle : String -> Html msg
viewSiteTitle text_ =
    div [ class "font-sitename font-bold mr-4" ] [ text text_ ]


viewNavBar : List (Html msg) -> List (Html msg) -> Html msg
viewNavBar items actions =
    nav [ class "w-full flex justify-between items-center bg-gray-200 p-4" ]
        [ div [ class "flex" ] items
        , div [ class "flex" ]
            actions
        ]


viewLink : String -> Html.Attribute msg -> Html msg
viewLink text_ attr =
    a [ class "text-blue-500 hover:underline cursor-pointer", attr ] [ text text_ ]


viewBoldLink : String -> Html.Attribute msg -> Html msg
viewBoldLink text_ attr =
    a [ class "text-blue-500 hover:underline cursor-pointer font-bold", attr ] [ text text_ ]


viewStringTable : List a -> List String -> List (a -> String) -> Html msg
viewStringTable objs headers fields =
    viewRawHtmlTable headers (List.map (\obj -> List.map (\field -> text (field obj)) fields) objs)


viewHtmlTable : List a -> List String -> List (a -> Html msg) -> Html msg
viewHtmlTable objs headers fields =
    viewRawHtmlTable headers (List.map (\obj -> List.map (\field -> field obj) fields) objs)


viewRawHtmlTable : List String -> List (List (Html msg)) -> Html msg
viewRawHtmlTable headers rows =
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
                                (\row ->
                                    tr []
                                        (List.map
                                            (\field ->
                                                td [ class "px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500" ] [ field ]
                                            )
                                            row
                                        )
                                )
                                rows
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


viewBreadcrumbs : List ( Routes.Route, String ) -> Html msg
viewBreadcrumbs links =
    let
        -- Split the links into all but the last one, and the last one
        allButLast =
            List.take (List.length links - 1) links

        last =
            List.drop (List.length links - 1) links
    in
    div [ class "flex space-x-1 text-gray-500 mr-4" ]
        (List.concatMap breadcrumbItem allButLast ++ List.concatMap lastBreadcrumbItem last)


breadcrumbItem : ( Routes.Route, String ) -> List (Html msg)
breadcrumbItem ( url, label ) =
    [ div [] [ a [ Routes.href url, class "text-blue-500 hover:text-blue-700" ] [ text label ] ]
    , div [] [ text "/" ]
    ]


lastBreadcrumbItem : ( Routes.Route, String ) -> List (Html msg)
lastBreadcrumbItem ( url, label ) =
    [ div [] [ text label ] ]


type alias ToolbarButton msg =
    { label : String
    , message : msg
    , isPrimary : Bool
    , condition : Bool
    , position : Position
    }


type Position
    = Left
    | Center
    | Right


viewToolbar : Html msg -> List (ToolbarButton msg) -> Html msg
viewToolbar status buttons =
    let
        leftButtons =
            List.filter (\b -> b.position == Left && b.condition) buttons

        centerButtons =
            List.filter (\b -> b.position == Center && b.condition) buttons

        rightButtons =
            List.filter (\b -> b.position == Right && b.condition) buttons

        displayButtons btns =
            List.map
                (\b ->
                    if b.isPrimary then
                        viewSmallPrimaryButton ( b.label, b.message )

                    else
                        viewSmallSecondaryButton ( b.label, b.message )
                )
                btns
    in
    div [ class "flex flex-row border border-stone-400 rounded p-1" ]
        [ div [ class "flex mr-2 text-sm" ]
            [ status ]
        , div
            [ class "flex flex-row grow justify-between" ]
            [ div [ class "flex space-x-2" ] (displayButtons leftButtons)
            , div [ class "flex space-x-2" ] (displayButtons centerButtons)
            , div [ class "flex space-x-2" ] (displayButtons rightButtons)
            ]
        ]
