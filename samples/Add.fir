circuit AbsCircuitWithDelays : 
  module AbsCircuitWithDelays : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<10>, out : UInt<15>}
    
    clock is invalid
    reset is invalid
    io is invalid

    reg r : UInt<15>, clock

    io.out <= r
    r <= add(add(add(add(add(io.in, io.in),add(io.in, io.in)),add(add(io.in, io.in),add(io.in, io.in))),add(add(add(io.in, io.in),add(io.in, io.in)),add(add(io.in, io.in),add(io.in, io.in)))),add(add(add(add(io.in, io.in),add(io.in, io.in)),add(add(io.in, io.in),add(io.in, io.in))),add(add(add(io.in, io.in),add(io.in, io.in)),add(add(io.in, io.in),add(io.in, io.in)))))
