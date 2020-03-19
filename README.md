# elm-2048

elm --help

elm-format src/Main.elm
elm make src/Main.elm
elm reactor


### Development 

* Googling out elm-2048 gave me a few repos. I had trails if I got stuck.
* I opened original [2048 source code](), tried to deconstruct the model, view and update part (as in elm architecture):
 
Action c, x,y
 switch
  swap: 
  # left: swap c[x][y], c[x][y+1]
  
  merge: c[x1][y1].update(c[x1][y1].value + c[x2][y2].value)
         c[x2][y2].update(0)
         # del(c[x2][y2])
         # c[x2][y2] = new(0)
  
  
swap cell1 cell2
    return cell2 cell1
merge cell1 cell2
    return cell1.update(cell1.value + cell2.value) cell2.update(0)
   
 
Grid Model -> Main Model
constMaxMerges = 1
size: 4
cells: [][]
getRandomAvailableCell
getAvailableCells
addRandomCell
 -> getRandomAvailableCell = cell::new(2)
action->left
    for i in size
        mergeCount = 0
        for j in 1:size
            if not c[i][j-1].isEmpty and c[i][j-1].value == c[i][j].value and mergeCount < constMaxMerges:
                c[i][j-1] c[i][j] = merge
            elif mergeCount > 0
                if j<size-mergeCount:
                    c[i][j] c[i][j-mergeCount] = swap c[i][j] c[i][j-mergeCount]
                else:
                    del c[i][j]
                    c[i][j] = Cell::new(0)
    
    addRandomCell
    
                
                

Cell:
value: of (0, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048) 0 means empty, can be made enum later
need methods like 
new(value)
update(value)
delete
isEmpty()


add a tile of 2 at random available cell (at cell value of 0, add a tile of 2)

action:
 left: 
 right:
 up:
 down:
 

 



