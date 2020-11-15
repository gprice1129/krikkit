# krikkit

## TODO

### Multiplayer exploration

- Networking
  - TCP message protocol describing state changes
  - start with an authoritative server model for simplicity
    - send messages corresponding to requested state changes
    - update state based on received messages
    - possible game loop, drawing immediately after receiving messages:
      1. process local input events, producing a candidate updated state
      2. send candidate update messages to server, if any
      3. receive definitive update messages from server, telling us how to update our state
      4. draw the updated state
      5. loop
  - maybe figure out ICE (STUN and/or TURN) for avoiding firewall issues
- Then a multiplayer demo?


### Architecture

#### Simulation

- Generalize the game loop to be an arbitrary simulation loop
  - `(simulation-loop state-present! state->state stop? state.initial)`
  - Factor out the game loop's termination condition (which is currently
    hard-coded to test `winning?`) as `stop?`
  - define a general clock that `state->state` may use to measure the time
    elapsed during each iteration (it may choose to `sleep`, or consider the
    delta-time as a parameter within `state->state`)
    - clock state would be stored as a component of the simulation state
- Move action history into state, and have restarts (optionally) clear the history
- Factor game state into two components: simulation state and platform/UI/IO state
  - the renderer is a component of the UI state
  - Invert `draw-zone` dependencies
    - `draw-zone` learns the scale from the renderer (passed as a parameter),
      but does not provide it to `entity->pict` since the renderer should have
      already defined an appropriate `entity->pict` for the given scale

#### Graphics

- Define more general drawing based on state and location
  - it's hard to define very general drawing procedures that work for all
    games, so don't impose a strict architecture, just provide some utilities
    that renderers can optionally be composed from
  - rather than having `draw-zone` provide individual tile entities to the
    game-specific `entity->pict`, we could provide the entire state, plus a
    particular tile location (i.e., a `(zone-id x y)`) to `state+loc->pict`
    - `draw-zone` is then only responsible for arranging the tile pictures
      produced for some viewed portion of a zone
      - portion defined by rectangle: `pos.top-left` and `pos.bottom-right`
        - `pos.bottom-right` is not inclusive, so `top-left = bottom-right`
          means nothing is drawn
    - could even bundle both state and zone-id into `state+loc->pict`, giving
      us `pos->pict`, where `pos` is a `vec2`, so that `draw-zone` doesn't have
      to consider these (it's just passing them around anyway)
      - `(define pos->pict (lambda (pos) (state+loc->pict st (loc zid pos))))`
      - in this case, just rename `draw-zone` to `draw-grid` instead
      - `(draw-grid dc scale pos.top-left pos.bottom-right pos->pict)`
  - flexible `state+loc->pict` rendering
    - support pattern-based overrides to handle particular entity combinations
      in a single tile
      - e.g., a pumpkin over a candle is drawn as a jack-o-lantern
    - support pattern-based overrides to handle multi-tile combinations
      - e.g., walls at corners drawn differently from walls between other walls
      - e.g., provide variance to an otherwise uniform, multi-tile surface, so
        that the repetition doesn't introduce unintended visual artifacts

#### Platform-specific UI

- Fix GUI-related issues, like resize
  - formally keep a game-space vs. screen-space translation
    - translate, scale, clip
  - support multiple windows, each potentially a different view into the world


### Tools

Design new tools to simplify creating new games/demos

- a tile painter that displays the tile with repetition, to notice ugly artifacts immediately
- rule-based definition of input/drawing translators
- rule-based definition of `(action, state) -> state` transformers
  - e.g., pushing a pumpkin that is against a wall or other pumpkin will not move it
  - 2-dimensional notation for describing local spatial situation patterns
    - e.g.,
      movement direction = right
      and local space =
        ```
        _ _ _ _
        _ F P X
        _ _ _ _
        ```
      where _ is ignored, F is the fish, P is a pumpkin, X is an unknown we do
      case analysis on to decide how to advance the state
      - we would typically want to respect symmetries, so we should be able to
        say that this single rule also suffices to describe the analogous
        situations for the other directions (up, down, left)
- world/zone editor
- procedural content generators


### Game design

- a 4X-in-space game with player-programmable automation/AI

- a social strategy game
  - rules that change each time you play
