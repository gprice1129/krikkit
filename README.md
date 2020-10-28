# krikkit

## TODO

- Define a representation-independent interface for worlds (like we have for zones now)
- Define helpers for moving between zones
- Define generic world/zone drawing and input controllers
  - translate keys/buttons to lists of game actions
  - translate tile entities to pictures, and compose them in some way (e.g., stacking them)
    - may want to include zone-id, x, y (and other state components?) as input to producing pictures
      - e.g., to provide variance to a uniform, multi-tile surface, so it looks less patchy
    - may provide pattern-based overrides to handle particular entity combinations
    - getting more complex, we may even want pattern-based overrides to handle multi-tile combinations
      - e.g., walls at corners may look different than walls between other walls
- Make a graphical multi-zone demo with portals linking the zones
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
- Design new tools for making games/demos based on our experience at this point
  - a tile painter that displays the tile with repetition, to notice ugly artifacts immediately
  - rule-based definition of input/drawing translators
  - rule-based definition of `(action, state) -> state` transformers
    - e.g., pushing a pumpkin that is against a wall or other pumpkin will not move it
  - world/zone editor
- Fix GUI-related issues, like resize
  - formally keep a game-space vs. screen-space translation
    - translate, scale, clip
  - support multiple windows, each potentially a different view into the world
