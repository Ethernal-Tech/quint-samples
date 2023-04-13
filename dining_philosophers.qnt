module dining_philosophers {
    type Philosopher = (int, str)   // id, state ("THINKING", "HUNGRY", "EATING")
    type Fork = int                 // state (0 - busy, 1 - free)
    var philosophers: List[Philosopher]  
    var forks: List[Fork]  

    def evenPhilosophers: List[Philosopher] = philosophers.select(p => p._1 % 2 == 0)
    def oddPhilosophers: List[Philosopher] = philosophers.select(p => p._1 % 2 != 0)

    def isEven(p: Philosopher): bool = p._1 % 2 == 0 // checks if philosopher's id is even
    def isAvailable(f: Fork): bool = f == 1

    def pickPhilosopher(list: List[Philosopher], len: int): Philosopher = {
        nondet idx = oneOf(0.to(len - 1))
        list[idx]
    }

    def hungryPhilosophers: List[Philosopher] = 
        philosophers.select(p => p._2 == "HUNGRY")

    def eatingPhilosophers: List[Philosopher] = 
        philosophers.select(p => p._2 == "EATING")

    def thinkingPhilosophers: List[Philosopher] =
        philosophers.select(p => p._2 == "THINKING")

    pure def mod(num: int, moduo: int): int = 
        if (num > 0 and num < moduo)
            num
        else {
            if (num > moduo)
                num % moduo
            else {
                if (num == 0)
                    0
                else
                    (num % moduo) + moduo
            }
                
        }


    val state = {
        philosophers: philosophers,
        forks: forks
    }

    action init = all {
        philosophers' = List((0, "THINKING"), (1, "THINKING"), (2, "THINKING"), (3, "THINKING"), (4, "THINKING")),
        forks' = List(1, 1, 1, 1, 1)
    }
    
    action takeForks(ph: Philosopher): bool = {
        if (isEven(ph)) {
            if (isAvailable(forks[mod(ph._1, 5)])) all {
                if (isAvailable(forks[mod((ph._1 - 1), 5)])) all {
                    forks' = forks.replaceAt(mod((ph._1), 5), 0).replaceAt(mod((ph._1 - 1), 5), 0),
                    philosophers' = philosophers.replaceAt(ph._1, (ph._1, "EATING")),
                } else all {
                    philosophers' = philosophers,
                    forks' = forks
                }
                
            } else all {
                philosophers' = philosophers,
                forks' = forks
            }
        } else {
            if (isAvailable(forks[mod(ph._1 - 1, 5)])) all {
                if (isAvailable(forks[mod(ph._1, 5)])) all {
                    forks' = forks.replaceAt(mod(ph._1, 5), 0).replaceAt(mod(ph._1 - 1, 5), 0),
                    philosophers' = philosophers.replaceAt(ph._1, (ph._1, "EATING"))
                } else all {
                    philosophers' = philosophers,
                    forks' = forks
                }
            } else all {
                philosophers' = philosophers,
                forks' = forks
            }
        }
    }

    action releaseForks(ph: Philosopher): bool = all {
        forks' = forks.replaceAt(mod((ph._1), 5), 1).replaceAt(mod((ph._1 - 1), 5), 1),
        philosophers' = philosophers.replaceAt(ph._1, (ph._1, "THINKING"))
    }

    action startEating = all {
        length(hungryPhilosophers) > 0,
        val hungry = hungryPhilosophers
        val ph = pickPhilosopher(hungry, length(hungry))
        takeForks(ph)
    }    

    action makeSomeoneHungry = all{
        length(thinkingPhilosophers) > 0,
        val thinking = thinkingPhilosophers
        val ph = pickPhilosopher(thinking, length(thinking))
        philosophers' = philosophers.replaceAt(ph._1, (ph._1, "HUNGRY")),
        forks' = forks
    }
    
    action stopEating = all {
        length(eatingPhilosophers) > 0,
        val eating = eatingPhilosophers
        val ph = pickPhilosopher(eating, length(eating))
        releaseForks(ph)
    }

    action step = any {
        startEating,
        stopEating,
        makeSomeoneHungry
    }

}