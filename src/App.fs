namespace RobsWorld

module App =

  open System
  open Feliz
  open Elmish
  open ChildApps
  open Styles
  open Feliz.UseElmish

  type Model = { children: ChildState list }

  type Msg =
    | ChildMessage of id: string * ChildMsg
    | Shuffle

  let private rng = Random()

  let init () =
    Styles.css
    { children = ChildApps.definitions |> List.map ChildApps.init }, Cmd.none

  let update msg model =
    match msg with
    | ChildMessage(id, childMsg) ->
      let updated =
        model.children
        |> List.map (fun child ->
          if child.definition.id = id then
            ChildApps.update childMsg child
          else
            child)

      { model with children = updated }, Cmd.none
    | Shuffle ->
      { model with
          children = model.children |> List.sortBy (fun _ -> rng.Next()) }, Cmd.none

  let private floatingHeader dispatch =
    let marqueeMessages =
      [ "Welcome to the wired wilds!"
        "Sign the glitter guestbook!"
        "Refreshments: Surge, Jolt, Crystal Pepsi!"
        "Press play on the mixtape, crank the modem!"
        "Keep calm and sparkle on."
        "Shake the snowglobe of cyberspace!" ]

    Html.header
      [ prop.style
          [ style.textAlign.center
            //style.padding "40px 0 16px 0"
            style.display.flex
            style.flexDirection.column
            style.alignItems.center
            style.gap 12 ]
        prop.children
          [ Html.h1
              [ prop.style
                  [ style.fontSize 48
                    style.color "#f5f5f5"
                    //style.textShadow "0 0 12px #ff00ff, 0 0 24px #00ffff"
                    style.animationName "neonBlink"
                    style.animationDuration 8
                    style.animationIterationCount.infinite ]
                prop.text "Rob's World Wide Weird" ]
            Html.p
              [ prop.style
                  [ style.fontSize 14
                    style.maxWidth 720
                    style.lineHeight 1.5
                    style.backgroundColor "rgba(0,0,0,0.4)"
                    style.padding 16
                    style.borderRadius 12
                    //style.border (sprintf "4px groove %s" "#ff77ff")
                    style.boxShadow (0, 0, "rgba(255, 105, 255, 0.5)") ]
                prop.text
                  "100 strange and sparkly Elmish microsites living together in one neon neighborhood. Click, tap, and mash until the modem sings!" ]
            Html.div
              [ prop.className "global-marquee"
                prop.children
                  [ Html.div
                      [ prop.className "global-marquee-track"
                        prop.children (
                          (marqueeMessages @ marqueeMessages)
                          |> List.map (fun message ->
                            Html.span [ prop.className "global-marquee-message"; prop.text message ])
                        ) ] ] ]
            Html.div
              [ prop.style
                  [ style.display.flex
                    style.gap 16
                    style.flexWrap.wrap
                    style.justifyContent.center ]
                prop.children
                  [ Html.span [ prop.text "✨"; prop.style [ style.fontSize 24 ] ]
                    Html.span [ prop.text "100 mini dreams"; prop.style [ style.fontSize 12 ] ]
                    Html.span [ prop.text "🪩"; prop.style [ style.fontSize 24 ] ]
                    Html.span [ prop.text "GeoCities forever"; prop.style [ style.fontSize 12 ] ]
                    Html.span [ prop.text "🛰️"; prop.style [ style.fontSize 24 ] ] ] ]
            Html.div
              [ prop.className "header-controls"
                prop.children
                  [ Html.button
                      [ prop.className "retro-button shuffle-button"
                        prop.onClick (fun _ -> dispatch Shuffle)
                        prop.text "Shuffle the Weird!" ] ] ] ] ]

  [<ReactComponent>]
  let Component () =

    let model, dispatch = React.useElmish(init, update)

    Html.div
      [ prop.children
          [ floatingHeader dispatch
            Html.main
              [ prop.className "app-wall"
                prop.children (
                  model.children
                  |> List.map (fun child ->
                    ChildApps.view child (fun msg -> dispatch (ChildMessage(child.definition.id, msg))))
                ) ] ] ]
