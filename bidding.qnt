module bidding {
    
    type Bidder = (
        int,    // id
        int,    // bid
        str,    // status
        int,    // amount of money
    )
    
    // state variables --------
    var bid1: Bidder
    var bid2: Bidder
    var bidWon: string
    var deadline: bool
    var bidders: List[Bidder]
    // ------------------------


    val MAX_BID = 100
    val MIN_BID = -100
    val MAX_AMOUNT = 1000

    def state = {
        bid1: bid1,
        bid2: bid2,
        bidWon: bidWon,
        deadline: deadline,
        bidders: bidders
    }

    // checking if both bidders have played a move
    def bothPlayersBidded: bool = all {
        // not(bid1._2 == -1), // superfluous
        // not(bid2._2 == -1), // superfluous
        // check whether the bids are valid
        bid1._2 > 0,
        bid2._2 > 0
    }

    // checking if both bidders were selected
    def bothPlayersPicked: bool = all {
        bid1._3 == "PLAYING",
        bid2._3 == "PLAYING"
    }

    // declares a winner
    def declareWinner(b1: int, b2: int): str = {
        if(b1 != b2) {
            if (b1 > b2)
                "bidder1"
            else
                "bidder2"
        } else {
            ""
        }
    }

    // returns a list of bidders who's status is "NOT_PLAYING"
    def availableBidders: List[Bidder] =
        bidders.select(b => b._3 == "NOT_PLAYING")

    // picks two random numbers from a provided list
    pure def pickTwoNumbers(s: Set[int]): List[int] = {
        nondet idx = oneOf(s)
        nondet idx2 = oneOf(s.filter(num => num != idx))
        [idx, idx2]
    }

    // iterates through free-bidders list and appends their ids to a set
    def getNumbersForSelection: Set[int] =
        availableBidders.foldl(Set(), (s, b) => s.union(Set(b._1)))

    
    action init = all {
        bidders' = List((0, -1, "NOT_PLAYING", oneOf(0.to(MAX_AMOUNT))), (1, -1, "NOT_PLAYING", oneOf(0.to(MAX_AMOUNT))), (2, -1, "NOT_PLAYING", oneOf(0.to(MAX_AMOUNT))), (3, -1, "NOT_PLAYING", oneOf(0.to(MAX_AMOUNT))), (4, -1, "NOT_PLAYING", oneOf(0.to(MAX_AMOUNT)))),
        bid1' = (-1, -1, "", -1),
        bid2' = (-1, -1, "", -1),
        bidWon' = "",
        deadline' = false
    }

    action pickBidders = all {
        nondet willPass = oneOf(0.to(1))    // unreliable communication
        willPass == 1,
        not(bothPlayersPicked),
        val idxs = pickTwoNumbers(getNumbersForSelection)
        all {
            bidders' = bidders.replaceAt(idxs[0], (idxs[0], -1, "PLAYING", bidders[idxs[0]]._4)).replaceAt(idxs[1], (idxs[1], -1, "PLAYING", bidders[idxs[1]]._4)),
            bid1' = (bidders[idxs[0]]._1, bidders[idxs[0]]._2, "PLAYING", bidders[idxs[0]]._4),
            bid2' = (bidders[idxs[1]]._1, bidders[idxs[1]]._2, "PLAYING", bidders[idxs[1]]._4),
            bidWon' = bidWon,
            deadline' = deadline
        }
    }


    action makeBid1 = all {
        nondet willPass = oneOf(0.to(1))
        willPass == 1,
        bothPlayersPicked,
        deadline == false,
        nondet b1 = oneOf(MIN_BID.to(MAX_BID))
        all {
            bid1._4 >= b1,
            bid1' = (bid1._1, b1, bid1._3, bid1._4)
        },
        bid2' = bid2,
        bidWon' = bidWon,
        deadline' = deadline,
        bidders' = bidders
    }

    action makeBid2 = all {
        nondet willPass = oneOf(0.to(1))
        willPass == 1,
        bothPlayersPicked,
        deadline == false,
        bid1' = bid1,
        nondet b2 = oneOf(MIN_BID.to(MAX_BID))
        all {
            bid2._4 >= b2,
            bid2' = (bid2._1, b2, bid2._3, bid2._4)
        },
        bidWon' = bidWon,
        deadline' = deadline,
        bidders' = bidders
    }

    // Should the money states be updated after the game is over?  It is not defined in a task
    action hitDeadline = all {
        nondet willPass = oneOf(0.to(1))
        willPass == 1,
        bothPlayersBidded,
        deadline == false,

        bid1' = bid1,
        bid2' = bid2,
        deadline' = true,
        bidWon' = declareWinner(bid1._2, bid2._2),
        bidders' = bidders
    }

    action startNewRound = all {
        nondet willPass = oneOf(0.to(1))
        willPass == 1,
        deadline == true,

        // similar as init, but amount of money of each player remains from the past round
        bidders' = List((0, -1, "NOT_PLAYING", bidders[0]._4), (1, -1, "NOT_PLAYING", bidders[1]._4), (2, -1, "NOT_PLAYING", bidders[2]._4), (3, -1, "NOT_PLAYING", bidders[3]._4), (4, -1, "NOT_PLAYING", bidders[4]._4)),
        bid1' = (-1, -1, "", -1),
        bid2' = (-1, -1, "", -1),
        bidWon' = "",
        deadline' = false
    }

    action step = any {
        pickBidders,
        makeBid1,
        makeBid2,
        hitDeadline,
        startNewRound
    }
    
}
