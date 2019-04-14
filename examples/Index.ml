let () = Stateful.App.mount ~at:"stateful"
let () = Composition.App.mount ~at:"composition"
let () = Timer.App.mount ~at:"timer"
let () = ManyInput.App.mount ~at:"many-input"

let () = Old_Stateless.mount ~at:"old-stateless"
let () = Old_Stateful.mount ~at:"old-stateful"
let () = Old_ReactInterop.mount ~at:"old-react-interop"
let () = Old_Composition.mount ~at:"old-composition"