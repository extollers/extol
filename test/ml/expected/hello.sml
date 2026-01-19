fun main_0 ctx  = (
write_1 ctx (String "Hello, world!") ;
nl_0 ctx );
val _ = Stack.try NONE (fn frame => main_0 {frame=frame})
