let () = Stateless.mount(~at="stateless");
let () = Stateful.mount(~at="stateful");
let () = ReactInterop.mount(~at="react-interop");
let () = Composition.mount(~at="composition");
let () = NoUpdate_Stateful.mount(~at="noupdate-stateful");
let () = NoUpdate_Composition.mount(~at="noupdate-composition");
let () = NoUpdate_Timer.mount(~at="noupdate-timer");