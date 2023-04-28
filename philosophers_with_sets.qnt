module philosophers_with_sets{
    
    
    type STATE = str

    val states : Set[STATE] = Set("THINKING", "HUNGRY", "EATING")

    pure val PHILOSOPHERS_NUM = 5

    type Fork = {id: int, status : bool}

    type Philosopher = {id : int, status: str}

    type Philosophers = Set[Philosopher]

    type Forks = Set[Fork]

    var philosophers: Philosophers

    var forks: Forks

    val state = {
        philosophers: philosophers,
        forks: forks
    }

    def updatePhilosopher(p_id: int, new_status: STATE): Philosophers = {
        val ph = philosophers.filter(p => p.id == p_id)
        val rm_p  = philosophers.exclude(ph)
        rm_p.union(Set({id: p_id, status: new_status}))
    }

    def updateForks(p_id: int, new_status: bool): Forks = {
        val p_lf = forks.exclude(forks.filter(f => f.id == p_id)).union(Set({id: p_id, status: new_status})) //update left fork first
        if(p_id == 1){
            p_lf.exclude(forks.filter(f => f.id == PHILOSOPHERS_NUM)).union(Set({id: PHILOSOPHERS_NUM, status: new_status}))
        }
        else{
            p_lf.exclude(forks.filter(f => f.id == p_id-1)).union(Set({id: p_id-1, status: new_status}))
        }
    }
    def checkLeftFork(p_id: int, status_check: bool): Forks = {
        forks.filter(f => f.id == p_id and f.status == status_check) //left fork
        
    }
    def checkRightFork(p_id: int, status_check: bool): Forks = {
        if(p_id == 1) {
            forks.filter(f => f.id == PHILOSOPHERS_NUM and f.status == status_check)
        }
        else {
            forks.filter(f => f.id == p_id-1 and f.status == status_check)
        }

    }

    action init = all {
        forks' = Set({id:1, status:true}, {id:2, status:true}, {id:3, status:true}, {id:4, status:true}, {id:5, status:true}),
        philosophers' = Set({id:1, status: "THINKING"}, {id:2, status: "THINKING"}, {id:3, status: "THINKING"}, {id:4, status: "THINKING"}, {id:5, status: "THINKING"}),

    }

    action make_someone_hungry(p_id: int) : bool = all {
        size(philosophers.filter(p => p.status == "THINKING" and p.id == p_id)) != 0,
        forks' = forks,
        philosophers' = updatePhilosopher(p_id, "HUNGRY")
    }

    action start_eating(p_id: int) : bool = all {
        
        val leftFork = checkLeftFork(p_id, true)
        val rightFork = checkRightFork(p_id, true)
        all{
            size(philosophers.filter(p => p.status == "HUNGRY" and p.id == p_id)) != 0,
            size(leftFork.union(rightFork)) == 2,
            philosophers' = updatePhilosopher(p_id, "EATING"),
            forks' = updateForks(p_id, false)
        }
    }

    action stop_eating(p_id: int) : bool = all {
        val leftFork = checkLeftFork(p_id, false)
        val rightFork = checkRightFork(p_id, false)
        all{
            size(philosophers.filter(p => p.status == "EATING" and p.id == p_id)) != 0,
            size(leftFork.union(rightFork)) == 2,
            philosophers' = updatePhilosopher(p_id, "THINKING"),
            forks' = updateForks(p_id, true)
        }
    }

    action step = {
            nondet p_h_id = 1.to(PHILOSOPHERS_NUM).oneOf()
            nondet p_e_id = 1.to(PHILOSOPHERS_NUM).oneOf()
            nondet p_se_id = 1.to(PHILOSOPHERS_NUM).oneOf()
            any {
                make_someone_hungry(p_h_id),
                start_eating(p_e_id),
                stop_eating(p_se_id)
            }
        }

    val forksAvailabilityNotZeroInv : bool = size(forks.filter(f => f.status == true)) > 0  
    

}
