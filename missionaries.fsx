open System.Collections.Generic

type NoMissionaries = int
type NoCannibals = int
type Boat = bool
type State = NoMissionaries * NoCannibals * Boat    // No of missionaries and cannibals, and boat, on the left side of the river
type StateHistory = State list
type Action = NoMissionaries * NoCannibals

let isValidState (m,c,_) = m > -1 && c > -1 && m < 4 && c < 4

let isGoalState (state:State) = 
    state = (0,0,false)     // 0 missionaries, 0 cannibals, and no boat on the left side of the river

let isDeadState (m,c,_) = (c>m && m>0) || (c<m && m<3)

let goLeft (m1,c1,b1) (m2,c2) = (m1+m2,c1+c2,not b1)

let goRight (m1,c1,b1) (m2,c2) = (m1-m2,c1-c2,not b1)

// Find all valid and unvisited successor states of the given state, aka transition model
let successors (state:State) (stateHistory:StateHistory) =
    let possibleActions:Action list = [ (1,0); (0,1); (2,0); (0,2); (1,1) ]
    let (_,_,boat) = state
    let doAction = if boat then goRight else goLeft
    possibleActions |> List.fold (fun acc action -> 
            let newState = doAction state action
            if (isValidState newState) && not (isDeadState newState) && not (List.contains newState stateHistory) then 
                newState::acc else acc) []

let initialState:State = (3,3,true)     // 3 missionaries, 3 cannibals, and boat on the left side of the river
let stateHistory:StateHistory = [initialState]

// Depth-first search using calling stack
let rec depthFirstSearch (state:State) (stateHistory:StateHistory) (results:StateHistory list) =
    if isGoalState state then (List.rev stateHistory)::results
    else
        successors state stateHistory
        |> List.fold (fun acc newState -> depthFirstSearch newState (newState::stateHistory) acc) results
   
let pathsDFS = depthFirstSearch initialState stateHistory []

// Breadth-first search using a queue
let breadthFirstSearch() =
    let rec bfs (fringe:Queue<State*StateHistory>) (results:List<StateHistory>) =
        if fringe.Count = 0 then results
        else
            let (state, stateHistory) = fringe.Dequeue()
            if isGoalState state then results.Add (List.rev stateHistory)
            else
                successors state stateHistory
                |> List.iter (fun newState -> fringe.Enqueue (newState,(newState::stateHistory)))
            bfs fringe results

    let fringe = new Queue<State*StateHistory>()
    fringe.Enqueue(initialState,[initialState])
    bfs fringe (new List<StateHistory>())

let pathsBFS = breadthFirstSearch() 
