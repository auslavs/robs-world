namespace RobsWorld

module ChildApps =

  open System
  open Feliz

  [<RequireQualifiedAccess>]
  type AnimationClass =
    | Glitter
    | Rainbow
    | Nebula
    | Gridline
    | Static
    | Laser
    | Confetti
    | Glitch
    | Hue
    | Vhs
    | Ocean
    | Scanlines
    | Disco
    | Aurora
    | Fire
    | Matrix
    | Candy
    | ZigZag
    | Vaporwave
    | Storm
    member this.CssClass =
      match this with
      | AnimationClass.Glitter -> "anim-glitter"
      | AnimationClass.Rainbow -> "anim-rainbow"
      | AnimationClass.Nebula -> "anim-nebula"
      | AnimationClass.Gridline -> "anim-gridline"
      | AnimationClass.Static -> "anim-static"
      | AnimationClass.Laser -> "anim-laser"
      | AnimationClass.Confetti -> "anim-confetti"
      | AnimationClass.Glitch -> "anim-glitch"
      | AnimationClass.Hue -> "anim-hue"
      | AnimationClass.Vhs -> "anim-vhs"
      | AnimationClass.Ocean -> "anim-ocean"
      | AnimationClass.Scanlines -> "anim-scanlines"
      | AnimationClass.Disco -> "anim-disco"
      | AnimationClass.Aurora -> "anim-aurora"
      | AnimationClass.Fire -> "anim-fire"
      | AnimationClass.Matrix -> "anim-matrix"
      | AnimationClass.Candy -> "anim-candy"
      | AnimationClass.ZigZag -> "anim-zigzag"
      | AnimationClass.Vaporwave -> "anim-vaporwave"
      | AnimationClass.Storm -> "anim-storm"

  type Theme =
    { background: string
      borderColor: string
      textColor: string
      accentColor: string
      animation: AnimationClass }

  type CounterConfig =
    { label: string
      step: int
      minValue: int
      maxValue: int
      hype: string }

  type TodoConfig =
    { placeholder: string
      mantra: string
      starter: string list }

  type MarqueeConfig =
    { messages: string list
      speed: int
      direction: string
      fontSize: int }

  type ColorCyclerConfig =
    { palette: string list
      slogan: string
      shimmer: string }

  type QuoteConfig =
    { name: string
      quotes: string list
      vibe: string }

  type ReactionConfig =
      { triggers: string list
        noises: string list
        alt: string }

  type PetConfig =
      { petName: string
        emoji: string
        hungerMax: int
        treat: string
        anthem: string }

  type PixelConfig =
      { size: int
        palette: string list
        legend: string }

  type StarConfig =
      { clusters: int
        label: string
        lore: string }

  type WaveConfig =
      { phrase: string
        amplitude: int
        speed: float
        font: string }

  type DjConfig =
      { pads: string list
        chant: string
        tempo: int }

  type ChildFlavor =
      | Counter of CounterConfig
      | Todo of TodoConfig
      | Marquee of MarqueeConfig
      | ColorCycler of ColorCyclerConfig
      | Quote of QuoteConfig
      | Reaction of ReactionConfig
      | Pet of PetConfig
      | Pixel of PixelConfig
      | Starfield of StarConfig
      | Wave of WaveConfig
      | Dj of DjConfig

  type ChildDefinition =
      { id: string
        title: string
        tagline: string
        theme: Theme
        flavor: ChildFlavor }

  type CounterModel =
      { value: int
        config: CounterConfig }

  type TodoItem =
      { text: string
        completed: bool }

  type TodoModel =
      { items: TodoItem list
        draft: string
        config: TodoConfig }

  type MarqueeModel =
      { index: int
        paused: bool
        config: MarqueeConfig }

  type ColorCyclerModel =
      { index: int
        config: ColorCyclerConfig }

  type QuoteModel =
      { index: int
        config: QuoteConfig }

  type ReactionModel =
      { hypeLevel: int
        last: string option
        config: ReactionConfig }

  type PetModel =
      { hunger: int
        affection: int
        config: PetConfig }

  type PixelModel =
      { pixels: string option array
        config: PixelConfig }

  type Star =
      { x: int
        y: int
        size: int
        twinkle: string }

  type StarModel =
      { stars: Star list
        config: StarConfig }

  type WaveModel =
      { phase: float
        config: WaveConfig }

  type DjPad =
      { label: string
        active: bool
        color: string }

  type DjModel =
      { pads: DjPad list
        beats: int
        config: DjConfig }

  type ChildModel =
      | CounterModel of CounterModel
      | TodoModel of TodoModel
      | MarqueeModel of MarqueeModel
      | ColorCyclerModel of ColorCyclerModel
      | QuoteModel of QuoteModel
      | ReactionModel of ReactionModel
      | PetModel of PetModel
      | PixelModel of PixelModel
      | StarModel of StarModel
      | WaveModel of WaveModel
      | DjModel of DjModel

  type ChildMsg =
      | CounterMsg of CounterMsg
      | TodoMsg of TodoMsg
      | MarqueeMsg of MarqueeMsg
      | ColorCyclerMsg of ColorCyclerMsg
      | QuoteMsg of QuoteMsg
      | ReactionMsg of ReactionMsg
      | PetMsg of PetMsg
      | PixelMsg of PixelMsg
      | StarMsg of StarMsg
      | WaveMsg of WaveMsg
      | DjMsg of DjMsg

  and CounterMsg =
      | Increment
      | Decrement
      | Reset
      | HyperJump

  and TodoMsg =
      | SetDraft of string
      | AddTodo
      | ToggleTodo of int
      | ShuffleTodos
      | ClearTodos

  and MarqueeMsg =
      | NextMessage
      | TogglePause

  and ColorCyclerMsg =
      | NextColor
      | PrevColor

  and QuoteMsg =
      | NextQuote
      | SurpriseQuote

  and ReactionMsg =
      | TriggerHype
      | Whisper

  and PetMsg =
      | Feed
      | Pet
      | Party

  and PixelMsg =
      | TogglePixel of int
      | ClearPixels
      | RandomizePixels

  and StarMsg =
      | ShuffleStars

  and WaveMsg =
      | Oscillate

  and DjMsg =
      | TogglePad of int
      | DropBeat

  let private neonPalettes : (Theme * AnimationClass) array =
      [|
        { background = "linear-gradient(135deg,#120458,#9827ff)"
          borderColor = "#ff66ff"
          textColor = "#ffffff"
          accentColor = "#ffed00"
          animation = AnimationClass.Disco }, AnimationClass.Disco
        { background = "linear-gradient(135deg,#21094e,#511281,#fbd1a2)"
          borderColor = "#ff7bac"
          textColor = "#fff0f6"
          accentColor = "#ff0066"
          animation = AnimationClass.Rainbow }, AnimationClass.Rainbow
        { background = "linear-gradient(135deg,#000428,#004e92)"
          borderColor = "#00f5d4"
          textColor = "#dff6ff"
          accentColor = "#ff9f1c"
          animation = AnimationClass.Ocean }, AnimationClass.Ocean
        { background = "linear-gradient(120deg,#4b145b,#6a00f4,#ff5da2)"
          borderColor = "#ffd23f"
          textColor = "#ffffff"
          accentColor = "#00f0b5"
          animation = AnimationClass.Glitter }, AnimationClass.Glitter
        { background = "linear-gradient(160deg,#0f172a,#312e81,#a855f7)"
          borderColor = "#f72585"
          textColor = "#f1f5f9"
          accentColor = "#4cc9f0"
          animation = AnimationClass.Nebula }, AnimationClass.Nebula
        { background = "linear-gradient(150deg,#000000,#434343)"
          borderColor = "#00ff95"
          textColor = "#ffffff"
          accentColor = "#ff477e"
          animation = AnimationClass.Matrix }, AnimationClass.Matrix
        { background = "linear-gradient(150deg,#1b003a,#3d0066,#ff00c1)"
          borderColor = "#ffdd00"
          textColor = "#ffe6ff"
          accentColor = "#00f5ff"
          animation = AnimationClass.Aurora }, AnimationClass.Aurora
        { background = "linear-gradient(135deg,#ff0844,#ffb199)"
          borderColor = "#ffe156"
          textColor = "#ffffff"
          accentColor = "#00e0ff"
          animation = AnimationClass.Fire }, AnimationClass.Fire
        { background = "linear-gradient(135deg,#283c86,#45a247)"
          borderColor = "#faff00"
          textColor = "#ecfdf5"
          accentColor = "#ff66c4"
          animation = AnimationClass.Hue }, AnimationClass.Hue
        { background = "linear-gradient(135deg,#2d0b45,#5f0f87,#a4508b)"
          borderColor = "#fdeff9"
          textColor = "#fff8f0"
          accentColor = "#f9c74f"
          animation = AnimationClass.Candy }, AnimationClass.Candy
        { background = "linear-gradient(135deg,#111,#333)"
          borderColor = "#ff6d00"
          textColor = "#ffffff"
          accentColor = "#00f0ff"
          animation = AnimationClass.Scanlines }, AnimationClass.Scanlines
        { background = "linear-gradient(120deg,#540d6e,#ee4266,#ffd23f,#3bceac,#0ead69)"
          borderColor = "#ffffff"
          textColor = "#ffffff"
          accentColor = "#120458"
          animation = AnimationClass.Vaporwave }, AnimationClass.Vaporwave
        { background = "linear-gradient(135deg,#42275a,#734b6d)"
          borderColor = "#fff200"
          textColor = "#fef6ff"
          accentColor = "#39ff14"
          animation = AnimationClass.Static }, AnimationClass.Static
        { background = "linear-gradient(120deg,#232526,#414345)"
          borderColor = "#d4af37"
          textColor = "#f9fafb"
          accentColor = "#00f6ff"
          animation = AnimationClass.Laser }, AnimationClass.Laser
        { background = "linear-gradient(135deg,#000,#1a1a1a,#444)"
          borderColor = "#b3ff00"
          textColor = "#f4f4f4"
          accentColor = "#ff00ea"
          animation = AnimationClass.Glitch }, AnimationClass.Glitch
        { background = "linear-gradient(135deg,#000428,#004e92)"
          borderColor = "#ffcb77"
          textColor = "#fefae0"
          accentColor = "#00f5d4"
          animation = AnimationClass.Storm }, AnimationClass.Storm
        { background = "linear-gradient(135deg,#ff9a9e,#fad0c4)"
          borderColor = "#ff85a1"
          textColor = "#382f32"
          accentColor = "#6a0572"
          animation = AnimationClass.ZigZag }, AnimationClass.ZigZag
      |]

  let private getTheme index =
    let theme, _ = neonPalettes.[index % neonPalettes.Length]
    theme

  let private getAnimation index =
    let _, anim = neonPalettes.[index % neonPalettes.Length]
    anim

  let private rngFromSeed seed =
    let mutable state = seed
    fun (max: int) ->
      state <- (state * 1664525 + 1013904223) &&& System.Int32.MaxValue
      state % max

  let private pickPalette index =
    let theme = getTheme index
    theme.background, theme.borderColor, theme.textColor, theme.accentColor, (getAnimation index).CssClass

  let private wrapIndex length index =
    if length = 0 then 0 else ((index % length) + length) % length

  let private createCounter id idx label step hype =
      let background, border, text, accent, animClass = pickPalette idx
      { id = id
        title = label
        tagline = hype
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor =
          ChildFlavor.Counter
              { label = label
                step = step
                minValue = -999
                maxValue = 999
                hype = hype } }

  let private createTodo id idx title mantra starter =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = mantra
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Todo { placeholder = "Type & smash enter"; mantra = mantra; starter = starter } }

  let private createMarquee id idx title direction speed messages tagline =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = tagline
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Marquee { messages = messages; speed = speed; direction = direction; fontSize = 12 } }

  let private createColorCycler id idx title slogan palette shimmer =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = slogan
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.ColorCycler { palette = palette; slogan = slogan; shimmer = shimmer } }

  let private createQuote id idx title vibe quotes =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = vibe
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Quote { name = title; quotes = quotes; vibe = vibe } }

  let private createReaction id idx title alt triggers noises =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = alt
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Reaction { triggers = triggers; noises = noises; alt = alt } }

  let private createPet id idx title config tagline =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = tagline
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Pet config }

  let private createPixel id idx title legend palette =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = legend
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Pixel { size = 8; palette = palette; legend = legend } }

  let private createStarfield id idx title label lore clusters =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = lore
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Starfield { clusters = clusters; label = label; lore = lore } }

  let private createWave id idx title phrase amplitude font =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = phrase
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Wave { phrase = phrase; amplitude = amplitude; speed = 0.6 + float amplitude / 20.; font = font } }

  let private createDj id idx title chant pads tempo =
      let background, border, text, accent, _ = pickPalette idx
      { id = id
        title = title
        tagline = chant
        theme =
          { background = background
            borderColor = border
            textColor = text
            accentColor = accent
            animation = getAnimation idx }
        flavor = ChildFlavor.Dj { pads = pads; chant = chant; tempo = tempo } }

  let definitions : ChildDefinition list =
      let counterDefs =
        [ for i in 1 .. 12 ->
            createCounter
              (sprintf "counter-%d" i)
              (i - 1)
              (sprintf "Quantum Counter #%d" i)
              (if i % 3 = 0 then 7 else if i % 3 = 1 then 3 else 5)
              (sprintf "Every click sends vibes x%d!" i) ]

      let todoDefs =
          [ for i in 1 .. 12 ->
              let mantra =
                [ "Make glitter happen"
                  "Dial the modem"
                  "Feed the Tamagotchi"
                  "Rewind the VHS"
                  "Polish the lava lamp" ]
                |> List.item ((i - 1) % 5)
              createTodo
                (sprintf "todo-%d" i)
                (12 + i)
                (sprintf "To-Do Turbo #%d" i)
                mantra
                [ "Wear neon socks"
                  "Build midi beats"
                  "Call the hotline"
                  "Decorate the guestbook" ] ]

      let marqueeDefs =
          [ for i in 1 .. 10 ->
                createMarquee
                    (sprintf "marquee-%d" i)
                    (24 + i)
                    (sprintf "Scrolling Legend #%d" i)
                    (if i % 2 = 0 then "left" else "right")
                    (12 + (i % 5) * 4)
                    [ "Welcome to Rob's Hyperspace!"
                      "Dial-up dreams and pixel beams"
                      "Click me, love me, sign my guestbook"
                      "Loading glitter... complete!"
                      "Beware: extreme radness ahead" ]
                    "Infinite ticker of cosmic nonsense" ]

      let cyclerDefs =
          [ for i in 1 .. 10 ->
                createColorCycler
                    (sprintf "color-%d" i)
                    (34 + i)
                    (sprintf "Palette Portal #%d" i)
                    "Rotate the rainbow dimension"
                    [ "#ff007f"; "#00f7ff"; "#ffee00"; "#ff9100"; "#b388ff" ]
                    "Flicker the spectrum" ]

      let quoteDefs =
          [ for i in 1 .. 10 ->
                createQuote
                    (sprintf "quote-%d" i)
                    (44 + i)
                    (sprintf "Retro Oracle #%d" i)
                    "Unlock wisdom from the void"
                    [ "404 fears not found."
                      "Friendship is the best hyperlink."
                      "Reality buffers while dreams download."
                      "Keep scrolling until the future loads."
                      "Press start to believe." ] ]

      let reactionDefs =
          [ for i in 1 .. 10 ->
                createReaction
                    (sprintf "reaction-%d" i)
                    (54 + i)
                    (sprintf "Hype Machine #%d" i)
                    "Pumps the crowd with ascii fireworks"
                    [ "HYPE"; "Y2K"; "ROBO"; "BASS"; "GLITCH" ]
                    [ "*airhorn*"; "boing!"; "vrmmm"; "psshhh"; "bing-bong" ] ]

      let petDefs =
          [ for i in 1 .. 8 ->
                let name = [| "Pixelpuff"; "GigaHam"; "LaserCat"; "FloppyFish"; "NeonPup"; "BitBat"; "RaveFrog"; "SynthSeal" |].[i - 1]
                createPet
                    (sprintf "pet-%d" i)
                    (64 + i)
                    (sprintf "Cyber Pet #%d" i)
                    { petName = name
                      emoji = [| "(=^.^=)"; "<(^.^<)"; "(>\"\"<)"; "(=^･^=)"; "(◕‿◕✿)"; "(•̀ᴗ•́)و"; "(°ᴗ°)"; "(｡♥‿♥｡)" |].[i - 1]
                      hungerMax = 8 + i
                      treat = "Mega Snack"
                      anthem = "Keep the disco ball spinning!" }
                    "Virtual critter needs constant glitter" ]

      let pixelDefs =
          [ for i in 1 .. 8 ->
                createPixel
                    (sprintf "pixel-%d" i)
                    (72 + i)
                    (sprintf "Pixel Painter #%d" i)
                    "Toggle a mini masterpiece"
                    [ "#ff77aa"; "#55ffee"; "#ffee55"; "#222"; "#ffffff" ] ]

      let starDefs =
          [ for i in 1 .. 8 ->
                createStarfield
                    (sprintf "star-%d" i)
                    (80 + i)
                    (sprintf "Starfield #%d" i)
                    "Constellation generator"
                    "Map the bulletin board skies"
                    (10 + i) ]

      let waveDefs =
          [ for i in 1 .. 6 ->
                createWave
                    (sprintf "wave-%d" i)
                    (88 + i)
                    (sprintf "Wave Text #%d" i)
                    "Type til you vibe"
                    (6 + i)
                    (if i % 2 = 0 then "Orbitron" else "Press Start 2P") ]

      let djDefs =
          [ for i in 1 .. 6 ->
                createDj
                    (sprintf "dj-%d" i)
                    (94 + i)
                    (sprintf "Midi Moshpit #%d" i)
                    "Tap pads for retro rhythms"
                    [ "Kick"; "Snare"; "Clap"; "Hat"; "Zap"; "Laser" ]
                    (90 + i * 5) ]

      counterDefs
      @ todoDefs
      @ marqueeDefs
      @ cyclerDefs
      @ quoteDefs
      @ reactionDefs
      @ petDefs
      @ pixelDefs
      @ starDefs
      @ waveDefs
      @ djDefs

  let private initCounter (config: CounterConfig) : CounterModel =
      { value = 0; config = config }

  let private initTodo (config: TodoConfig) : TodoModel =
      { items = config.starter |> List.mapi (fun i text -> { text = text; completed = i % 2 = 0 })
        draft = ""
        config = config }

  let private initMarquee (config: MarqueeConfig) : MarqueeModel =
      { index = 0; paused = false; config = config }

  let private initCycler (config: ColorCyclerConfig) : ColorCyclerModel =
      { index = 0; config = config }

  let private initQuote (config: QuoteConfig) : QuoteModel =
      { index = 0; config = config }

  let private initReaction (config: ReactionConfig) : ReactionModel =
      { hypeLevel = 0; last = None; config = config }

  let private initPet (config: PetConfig) : PetModel =
      { hunger = config.hungerMax / 2; affection = 0; config = config }

  let private initPixel (config: PixelConfig) : PixelModel =
      { pixels = Array.init (config.size * config.size) (fun _ -> None); config = config }

  let private initStar (config: StarConfig) id : StarModel =
      let random = rngFromSeed (hash id)
      let stars =
          [ for i in 0 .. config.clusters - 1 ->
                { x = random 200
                  y = random 200
                  size = 2 + random 6
                  twinkle = [ "*"; "✶"; "✷"; "✸"; "✹"; "✺"; "✻"; "✼" ].[random 8] } ]
      { stars = stars; config = config }

  let private initWave (config: WaveConfig) : WaveModel =
      { phase = 0.; config = config }

  let private initDj (config: DjConfig) : DjModel =
      let random = rngFromSeed (hash config.tempo)
      { pads = config.pads |> List.mapi (fun i label -> { label = label; active = (random 2 = 1); color = [ "#ff2d95"; "#42f5ef"; "#ffe066"; "#845ef7"; "#ff6b6b"; "#51cf66" ].[i % 6] })
        beats = 0
        config = config }

  type ChildState =
      { definition: ChildDefinition
        model: ChildModel }

  let init definition =
      let model =
          match definition.flavor with
          | ChildFlavor.Counter config -> CounterModel (initCounter config)
          | ChildFlavor.Todo config -> TodoModel (initTodo config)
          | ChildFlavor.Marquee config -> MarqueeModel (initMarquee config)
          | ChildFlavor.ColorCycler config -> ColorCyclerModel (initCycler config)
          | ChildFlavor.Quote config -> QuoteModel (initQuote config)
          | ChildFlavor.Reaction config -> ReactionModel (initReaction config)
          | ChildFlavor.Pet config -> PetModel (initPet config)
          | ChildFlavor.Pixel config -> PixelModel (initPixel config)
          | ChildFlavor.Starfield config -> StarModel (initStar config definition.id)
          | ChildFlavor.Wave config -> WaveModel (initWave config)
          | ChildFlavor.Dj config -> DjModel (initDj config)
      { definition = definition; model = model }

  let private updateCounter msg (model: CounterModel) =
      match msg with
      | Increment -> { model with value = min model.config.maxValue (model.value + model.config.step) }
      | Decrement -> { model with value = max model.config.minValue (model.value - model.config.step) }
      | Reset -> { model with value = 0 }
      | HyperJump ->
          let jump = model.config.step * (abs (hash model.config.label) % 8 + 2)
          { model with value = model.value + jump }

  let private updateTodo msg (model: TodoModel) =
      match msg with
      | SetDraft text -> { model with draft = text }
      | AddTodo when String.IsNullOrWhiteSpace model.draft -> model
      | AddTodo ->
          let newItem = { text = model.draft; completed = false }
          { model with items = newItem :: model.items; draft = "" }
      | ToggleTodo index ->
          { model with
              items =
                  model.items
                  |> List.mapi (fun i item -> if i = index then { item with completed = not item.completed } else item) }
      | ShuffleTodos ->
          let random = rngFromSeed (hash (String.concat "" (model.items |> List.map (fun item -> item.text))))
          { model with items = model.items |> List.sortBy (fun _ -> random 1000) }
      | ClearTodos -> { model with items = [] }

  let private updateMarquee msg (model: MarqueeModel) =
      match msg with
      | NextMessage when model.config.messages.IsEmpty -> model
      | NextMessage ->
          let count = model.config.messages |> List.length
          { model with index = wrapIndex count (model.index + 1) }
      | TogglePause -> { model with paused = not model.paused }

  let private updateCycler msg (model: ColorCyclerModel) =
      let palette = model.config.palette
      let length = palette |> List.length
      match msg with
      | NextColor -> { model with index = wrapIndex length (model.index + 1) }
      | PrevColor -> { model with index = wrapIndex length (model.index - 1) }

  let private updateQuote msg (model: QuoteModel) =
      let length = model.config.quotes |> List.length
      match msg with
      | NextQuote -> { model with index = wrapIndex length (model.index + 1) }
      | SurpriseQuote ->
          let random = rngFromSeed (hash model.config.name)
          { model with index = random length }

  let private updateReaction msg (model: ReactionModel) =
      match msg with
      | TriggerHype ->
          let random = rngFromSeed (hash model.config.alt)
          let noises = model.config.noises
          let noise = noises.[random noises.Length]
          { model with
              hypeLevel = model.hypeLevel + 1
              last = Some noise }
      | Whisper -> { model with hypeLevel = max 0 (model.hypeLevel - 1); last = Some "pssst..." }

  let private updatePet msg (model: PetModel) =
      match msg with
      | Feed -> { model with hunger = max 0 (model.hunger - 2); affection = model.affection + 1 }
      | Pet -> { model with affection = model.affection + 2 }
      | Party ->
          { model with hunger = min model.config.hungerMax (model.hunger + 1); affection = model.affection + 3 }

  let private updatePixel msg (model: PixelModel) =
      match msg with
      | TogglePixel index ->
          let palette = model.config.palette |> List.toArray
          let color = palette.[index % palette.Length]
          let newPixels = Array.copy model.pixels
          newPixels.[index] <- if model.pixels.[index].IsSome then None else Some color
          { model with pixels = newPixels }
      | ClearPixels -> { model with pixels = Array.init model.pixels.Length (fun _ -> None) }
      | RandomizePixels ->
          let palette = model.config.palette |> List.toArray
          let random = rngFromSeed (hash model.config.legend)
          { model with
              pixels =
                  model.pixels
                  |> Array.mapi (fun _ _ -> if random 4 = 0 then Some palette.[random palette.Length] else None) }

  let private updateStar msg (model: StarModel) =
      match msg with
      | ShuffleStars ->
          let random = rngFromSeed (hash model.config.label + model.stars.Length)
          { model with
              stars =
                  model.stars
                  |> List.map (fun _ ->
                      { x = random 200
                        y = random 200
                        size = 2 + random 6
                        twinkle = [ "✶"; "✷"; "✸"; "✹"; "✺"; "✻"; "✼"; "✽" ].[random 8] }) }

  let private updateWave msg (model: WaveModel) =
      match msg with
      | Oscillate -> { model with phase = model.phase + model.config.speed }

  let private updateDj msg (model: DjModel) =
      match msg with
      | TogglePad index ->
          { model with
              pads =
                  model.pads
                  |> List.mapi (fun i pad -> if i = index then { pad with active = not pad.active } else pad) }
      | DropBeat -> { model with beats = model.beats + 1 }

  let update msg state =
      let model =
          match state.model, msg with
          | CounterModel counter, CounterMsg counterMsg -> CounterModel (updateCounter counterMsg counter)
          | TodoModel todo, TodoMsg todoMsg -> TodoModel (updateTodo todoMsg todo)
          | MarqueeModel marquee, MarqueeMsg marqueeMsg -> MarqueeModel (updateMarquee marqueeMsg marquee)
          | ColorCyclerModel cycler, ColorCyclerMsg cyclerMsg -> ColorCyclerModel (updateCycler cyclerMsg cycler)
          | QuoteModel quote, QuoteMsg quoteMsg -> QuoteModel (updateQuote quoteMsg quote)
          | ReactionModel reaction, ReactionMsg reactionMsg -> ReactionModel (updateReaction reactionMsg reaction)
          | PetModel pet, PetMsg petMsg -> PetModel (updatePet petMsg pet)
          | PixelModel pixel, PixelMsg pixelMsg -> PixelModel (updatePixel pixelMsg pixel)
          | StarModel stars, StarMsg starMsg -> StarModel (updateStar starMsg stars)
          | WaveModel wave, WaveMsg waveMsg -> WaveModel (updateWave waveMsg wave)
          | DjModel dj, DjMsg djMsg -> DjModel (updateDj djMsg dj)
          | _ -> state.model
      { state with model = model }

  let private themedBox (theme: Theme) (children: ReactElement list) =
      Html.div [
          prop.className (sprintf "child-app %s" theme.animation.CssClass)
          prop.style [
              style.backgroundColor theme.background
              style.borderColor theme.borderColor
              style.color theme.textColor
              style.boxShadow (0, 0, 20, theme.accentColor)
          ]
          prop.children children
      ]

  let private retroButton accent (label: string) dispatch msg =
      Html.button [
          prop.className "retro-button"
          prop.style [
              style.backgroundColor accent
              style.color "#050505"
          ]
          prop.onClick (fun _ -> dispatch msg)
          prop.text label
      ]

  let private counterView definition (model: CounterModel) dispatch =
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [
              prop.style [ style.fontSize 48; style.textAlign.center; style.padding (length.px 8) ]
              prop.text (string model.value)
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween ]
              prop.children [
                  retroButton definition.theme.accentColor "-" dispatch (CounterMsg Decrement)
                  retroButton definition.theme.accentColor "+" dispatch (CounterMsg Increment)
                  retroButton definition.theme.accentColor "Warp" dispatch (CounterMsg HyperJump)
                  retroButton definition.theme.accentColor "Reset" dispatch (CounterMsg Reset)
              ]
          ]
          Html.div [ prop.className "child-footer"; prop.text definition.tagline ]
      ]

  let private todoView definition (model: TodoModel) dispatch =
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.p [ prop.text definition.tagline; prop.style [ style.fontSize 12 ] ]
          Html.input [
              prop.className "retro-input"
              prop.placeholder model.config.placeholder
              prop.valueOrDefault model.draft
              prop.onChange (fun value -> dispatch (TodoMsg (SetDraft value)))
              prop.onKeyDown (fun ev -> if ev.key = "Enter" then dispatch (TodoMsg AddTodo))
          ]
          Html.ul [
              prop.style [ style.listStyleType.none; style.paddingLeft 0; style.fontSize 11; style.lineHeight 1.4 ]
              prop.children (
                  model.items
                  |> List.mapi (fun i item ->
                      Html.li [
                          prop.style [
                              style.textDecorationLine (if item.completed then textDecorationLine.lineThrough else textDecorationLine.none)
                              style.opacity (if item.completed then 0.6 else 1.0)
                              style.cursor.pointer
                          ]
                          prop.onClick (fun _ -> dispatch (TodoMsg (ToggleTodo i)))
                          prop.text item.text
                      ])
              )
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Shuffle" dispatch (TodoMsg ShuffleTodos)
                  retroButton definition.theme.accentColor "Clear" dispatch (TodoMsg ClearTodos)
              ]
          ]
      ]

  let private marqueeView definition (model: MarqueeModel) dispatch =
      let message =
          if model.config.messages.IsEmpty then "" else model.config.messages.[wrapIndex (List.length model.config.messages) model.index]
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [
              prop.style [
                  style.overflow.hidden
                  style.border (length.px 2, borderStyle.dashed, definition.theme.accentColor)
                  style.height 80
                  style.position.relative
                  style.fontSize (model.config.fontSize)
                  style.letterSpacing 2
              ]
              prop.children [
                  Html.div [
                      prop.style [
                          style.position.absolute
                          style.whitespace.nowrap
                          style.animationDuration model.config.speed
                          style.animationName "marqueeScroll"
                          style.animationIterationCount.infinite
                          if model.paused then style.animationPlayState.paused else style.animationPlayState.running
                          style.animationTimingFunction.linear
                          if model.config.direction = "left" then style.left (length.perc 100) else style.right (length.perc 100)
                          if model.config.direction = "left" then style.animationDirection.normal else style.animationDirection.reverse
                      ]
                      prop.text message
                  ]
              ]
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Next" dispatch (MarqueeMsg NextMessage)
                  retroButton definition.theme.accentColor (if model.paused then "Play" else "Pause") dispatch (MarqueeMsg TogglePause)
              ]
          ]
      ]

  let private colorCyclerView definition (model: ColorCyclerModel) dispatch =
      let palette = model.config.palette
      let count = palette |> List.length
      let color = palette.[wrapIndex count model.index]
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [
              prop.style [
                  style.height 100
                  style.borderRadius 12
                  style.marginBottom 12
                  style.border (length.px 4, borderStyle.groove, color)
                  style.backgroundColor color
                  style.boxShadow (0, 0, 24, color)
              ]
          ]
          Html.div [ prop.style [ style.fontSize 12; style.textAlign.center ]; prop.text color ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Prev" dispatch (ColorCyclerMsg PrevColor)
                  retroButton definition.theme.accentColor "Next" dispatch (ColorCyclerMsg NextColor)
              ]
          ]
      ]

  let private quoteView definition (model: QuoteModel) dispatch =
      let quotes = model.config.quotes
      let quote = quotes.[wrapIndex (List.length quotes) model.index]
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.p [ prop.style [ style.fontSize 12; style.lineHeight 1.6 ]; prop.text definition.tagline ]
          Html.blockquote [
              prop.style [
                  style.fontStyle.italic
                  style.backgroundColor "rgba(0,0,0,0.35)"
                  style.padding 12
                  style.borderLeft (length.px 4, borderStyle.solid, definition.theme.accentColor)
              ]
              prop.text quote
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Next" dispatch (QuoteMsg NextQuote)
                  retroButton definition.theme.accentColor "Surprise" dispatch (QuoteMsg SurpriseQuote)
              ]
          ]
      ]

  let private reactionView definition (model: ReactionModel) dispatch =
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [
              prop.style [ style.fontSize 36; style.textAlign.center; style.padding 12 ]
              prop.text (model.last |> Option.defaultValue "Let's GO!")
          ]
          Html.progress [
              prop.value model.hypeLevel
              prop.max 50
              prop.style [ style.width (length.perc 100); style.height 16 ]
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Hype" dispatch (ReactionMsg TriggerHype)
                  retroButton definition.theme.accentColor "Calm" dispatch (ReactionMsg Whisper)
              ]
          ]
      ]

  let private petView definition (model: PetModel) dispatch =
      let hungerPerc = float model.hunger / float model.config.hungerMax * 100.0
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text (sprintf "%s: %s" definition.title model.config.petName) ]
          Html.div [ prop.style [ style.fontSize 32; style.textAlign.center ]; prop.text model.config.emoji ]
          Html.div [ prop.style [ style.fontSize 12; style.textAlign.center ]; prop.text definition.tagline ]
          Html.div [ prop.style [ style.marginTop 8 ]; prop.text (sprintf "Hunger: %d/%d" model.hunger model.config.hungerMax) ]
          Html.div [ prop.style [ style.marginTop 4 ]; prop.text (sprintf "Affection: %d" model.affection) ]
          Html.div [
              prop.style [ style.display.flex; style.gap 8; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Feed" dispatch (PetMsg Feed)
                  retroButton definition.theme.accentColor "Pet" dispatch (PetMsg Pet)
                  retroButton definition.theme.accentColor "Party" dispatch (PetMsg Party)
              ]
          ]
          Html.div [
              prop.style [
                  style.marginTop 12
                  style.height 10
                  style.border (length.px 2, borderStyle.solid, definition.theme.accentColor)
              ]
              prop.children [
                  Html.div [
                      prop.style [
                          style.width (length.perc hungerPerc)
                          style.height (length.perc 100)
                          style.backgroundColor definition.theme.accentColor
                      ]
                  ]
              ]
          ]
      ]

  let private pixelView definition (model: PixelModel) dispatch =
      let size = model.config.size
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [ prop.style [ style.fontSize 10 ]; prop.text definition.tagline ]
          Html.div [
              prop.className "pixel-grid"
              prop.style [
                  style.gridTemplateColumns (Array.replicate size (length.fr 1))
                  style.gridTemplateRows (Array.replicate size (length.px 24))
              ]
              prop.children (
                  model.pixels
                  |> Array.mapi (fun i cell ->
                      Html.div [
                          prop.className "pixel-cell"
                          prop.style [
                              style.backgroundColor (cell |> Option.defaultValue "rgba(0,0,0,0.3)")
                              style.boxShadow (0, 0, 6, definition.theme.accentColor)
                          ]
                          prop.onClick (fun _ -> dispatch (PixelMsg (TogglePixel i)))
                      ])
                  |> Array.toList
              )
          ]
          Html.div [
              prop.style [ style.display.flex; style.justifyContent.spaceBetween; style.marginTop 12 ]
              prop.children [
                  retroButton definition.theme.accentColor "Clear" dispatch (PixelMsg ClearPixels)
                  retroButton definition.theme.accentColor "Chaos" dispatch (PixelMsg RandomizePixels)
              ]
          ]
      ]

  let private starView definition (model: StarModel) dispatch =
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [ prop.style [ style.fontSize 10; style.marginBottom 8 ]; prop.text definition.tagline ]
          Html.div [
              prop.style [
                  style.height 140
                  style.border (length.px 3, borderStyle.dashed, definition.theme.accentColor)
                  style.position.relative
                  style.backgroundColor "rgba(0,0,0,0.4)"
                  style.overflow.hidden
              ]
              prop.children (
                  model.stars
                  |> List.map (fun star ->
                      Html.span [
                          prop.style [
                              style.position.absolute
                              style.left (length.px star.x)
                              style.top (length.px star.y)
                              style.fontSize (star.size |> float |> length.px)
                              style.animationDelay (TimeSpan.FromSeconds(float (star.x + star.y) / 200.0))
                              style.animationName "neonBlink"
                              style.animationDuration (TimeSpan.FromSeconds(4.0))
                              style.animationIterationCount.infinite
                          ]
                          prop.text star.twinkle
                      ])
              )
          ]
          Html.div [ prop.style [ style.marginTop 12 ]; prop.text model.config.lore ]
          retroButton definition.theme.accentColor "Remix Sky" dispatch (StarMsg ShuffleStars)
      ]

  let private waveView definition (model: WaveModel) dispatch =
      let letters = definition.tagline |> Seq.toArray
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [
              prop.style [ style.display.flex; style.gap 4; style.flexWrap.wrap; style.fontFamily model.config.font ]
              prop.children (
                  letters
                  |> Array.mapi (fun i ch ->
                      Html.span [
                          prop.style [
                              style.display.inlineBlock
                              style.transform [
                                  transform.translateY (length.px (int (sin (model.phase + float i / 2.) * float model.config.amplitude)))
                              ]
                              style.transitionProperty "transform"
                              style.transitionDuration (TimeSpan.FromSeconds(0.3))
                          ]
                          prop.text (string ch)
                      ])
                  |> Array.toList
              )
          ]
          Html.div [ prop.style [ style.marginTop 12 ]; prop.text "Oscillate the vibe" ]
          retroButton definition.theme.accentColor "Wobble" dispatch (WaveMsg Oscillate)
      ]

  let private djView definition (model: DjModel) dispatch =
      themedBox definition.theme [
          Html.h3 [ prop.className "child-title"; prop.text definition.title ]
          Html.div [ prop.text definition.tagline; prop.style [ style.fontSize 11 ] ]
          Html.div [
              prop.style [ style.display.grid; style.gridTemplateColumns [| length.fr 1; length.fr 1; length.fr 1 |]; style.gap 8; style.marginTop 12 ]
              prop.children (
                  model.pads
                  |> List.mapi (fun i pad ->
                      Html.button [
                          prop.className "retro-button"
                          prop.style [
                              style.backgroundColor (if pad.active then pad.color else "rgba(0,0,0,0.4)")
                              style.color (if pad.active then "#050505" else definition.theme.textColor)
                              style.borderColor definition.theme.accentColor
                              style.height 48
                          ]
                          prop.text pad.label
                          prop.onClick (fun _ -> dispatch (DjMsg (TogglePad i)))
                      ])
              )
          ]
          Html.div [ prop.style [ style.marginTop 8 ]; prop.text (sprintf "Beats dropped: %d" model.beats) ]
          retroButton definition.theme.accentColor "Drop Beat" dispatch (DjMsg DropBeat)
      ]

  let view state dispatch =
      match state.model with
      | CounterModel model -> counterView state.definition model dispatch
      | TodoModel model -> todoView state.definition model dispatch
      | MarqueeModel model -> marqueeView state.definition model dispatch
      | ColorCyclerModel model -> colorCyclerView state.definition model dispatch
      | QuoteModel model -> quoteView state.definition model dispatch
      | ReactionModel model -> reactionView state.definition model dispatch
      | PetModel model -> petView state.definition model dispatch
      | PixelModel model -> pixelView state.definition model dispatch
      | StarModel model -> starView state.definition model dispatch
      | WaveModel model -> waveView state.definition model dispatch
      | DjModel model -> djView state.definition model dispatch