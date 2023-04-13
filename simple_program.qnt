
module simple_program {

    var pc: str // track program execution
    var i: int // counter

    // a state initializer
    action init = all {
      pc' = "start",
      i'= 0
    }

    val programState = {
      pc: pc,
      i: i,
    }
    // pick random number between 1 and 100
    action pick = all {
      pc == "start",
      i' = oneOf((1).to(100)),
      pc' = "middle"
    }

    action add1 = all {
      pc == "middle",
      i' = i + 1,
      pc' = "done"
    }

    action step = any {
      pick,
      add1
    }

    run test1 = init.then(step)
}
