class DraggablePile{
    constructor(template){
        this.element = draggablePileElement.cloneNode(true);
        this.template = template;
        this.element.style.backgroundColor = template.color;
        this.element.innerHTML = template.label;

        this.element.addEventListener('mousedown', (e) => {
            // Create a new draggable item
            let g = globalPos(e)
            let draggable = new DraggableNode(g.x, g.y, this);
            isDragging = true;
            currentDraggable = draggable;
        });
    }
}

class DraggableNode{
    constructor(x, y, parentPile){
        const draggable = draggableElement.cloneNode(true);
    
        this.element = draggable;
        this.parentPile = parentPile;
        this.element.style.backgroundColor = parentPile.template.color;

        draggable.textContent = parentPile.template.label;
        composeArea.appendChild(draggable);
    
        draggable.style.left = `${x - draggableHalfSize.w}px`; 
        draggable.style.top = `${y - draggableHalfSize.h}px`;  
    
        //this never happens because the user currently cant mousedown on this since it gets remove on mousedown
        // draggable.addEventListener('mousedown', (e) => {
        //     isDragging = true;
        //     currentDraggable = this;
        // });
    }

    remove(){
        this.element.remove();
    }
}

//---------------------------------------------------------------------------
//grid
let snapPoints = Array.from(document.querySelectorAll(".trow-visual"))
let centerPoints = Array.from(document.querySelectorAll(".crow"))

function addcrow(t){
    let index = snapPoints.indexOf(t);
    if (index > -1) {
        snapPoints.splice(index, 1); 
    }
    centerPoints.push(t);
    t.parentNode.classList.remove("trow");
    t.parentNode.classList.add("crow");
    fixCrow(t.parentNode);
    return t;
}

function removeCrow(c){
    let index = centerPoints.indexOf(c);
    if (index > -1) {
        centerPoints.splice(index, 1); 
    }

    c.parentNode.classList.remove("crow");
    c.parentNode.classList.add("trow");

    let col = c.parentNode.parentNode;

    fixTcol(col.nextElementSibling);
    fixTcol(col.previousElementSibling);
    fixTcol(col);

    if(col.childElementCount == 1 && col.firstElementChild.classList.contains("trow")){
        col.nextElementSibling.remove();
        col.previousElementSibling.remove();
    }

    updateConnections();
}

function fixTcol(t){
    //considering that all cells have either a trow or a crow class

    let currChild = t.firstElementChild;
    while(currChild = currChild.nextElementSibling){
        if(currChild.classList.contains("trow")){
            if(currChild.previousElementSibling.classList.contains("trow")){
                currChild.previousElementSibling.remove();
            }
        }else{
            if(currChild.previousElementSibling.classList.contains("crow")){
                fixCrow(currChild);
            }
        }
    }
}

function fixCrow(c){
    //up
    if(!(c.previousElementSibling && c.previousElementSibling.classList.contains("trow"))){
        c.parentNode.insertBefore(newTrow(), c);
    }
        
    //down
    if(!c.nextElementSibling){
        c.parentNode.appendChild(newTrow(), c);
    }else if(!c.nextElementSibling.classList.contains("trow")){
        c.parentNode.insertBefore(newTrow(), c.nextElementSibling);
    }

    //left
    if(!(c.parentNode.previousElementSibling
        && c.parentNode.previousElementSibling.childElementCount == 1
        && c.parentNode.previousElementSibling.firstElementChild.classList.contains("trow"))){
            c.parentNode.parentNode.insertBefore(newTcol(), c.parentNode);
    }

    //right
    if(!c.parentNode.nextElementSibling){
        c.parentNode.parentNode.appendChild(newTcol(), c.parentNode);
    }else if(!(c.parentNode.nextElementSibling.childElementCount == 1
        && c.parentNode.nextElementSibling.firstElementChild.classList.contains("trow"))){
        c.parentNode.parentNode.insertBefore(newTcol(), c.parentNode.nextElementSibling);
    }
}

function newTrow(){
    const t = document.createElement("div");
    t.classList.add("row", "trow");
    const v = document.createElement("div");
    v.classList.add("trow-visual");
    t.appendChild(v);
    snapPoints.push(v);
    return t;
}

function newTcol(){
    const t = document.createElement("div");
    t.classList.add("col");
    t.classList.add("tcol");
    t.appendChild(newTrow());
    return t;
}

//--------------------------------------------------------------------------------------
//snap

const snapThreshold = 30; // Distance threshold for snapping

let currentDraggable = null;
let isDragging = false;

const draggableHalfSize = {w: 50, h: 50}

document.addEventListener('mousemove', (e) => {
    updateDraggablePosition(e);
});

document.addEventListener('mouseup', () => {
    if (currentDraggable) {
        const { offsetLeft: left, offsetTop: top } = currentDraggable.element;

        let snapPoint = checkSnap(left, top)

        if (!snapPoint) {
            currentDraggable.remove();
        }else{
            snapCallback(snapPoint);
        }

        isDragging = false;
        currentDraggable = null;
    }
});

function checkSnap(x, y) {
    let snapped = null;
    snapPoints.forEach(point => {
        const pointRect = point.getBoundingClientRect();
        const draggableRect = currentDraggable.element.getBoundingClientRect();
        
        const pointX = pointRect.left - (draggableRect.width / 2) + window.scrollX;
        const pointY = pointRect.top - (draggableRect.height / 2) + window.scrollY;

        const dist = Math.sqrt((x - pointX) ** 2 + (y - pointY) ** 2);

        if (dist < snapThreshold) {
            currentDraggable.element.style.left = `${pointX}px`;
            currentDraggable.element.style.top = `${pointY}px`;
            snapped = point;
        }
    });

    return snapped;
}

function snapCallback(snapPoint){
    let c = addcrow(snapPoint);
    handleSnap(c, currentDraggable)
}

function handleSnap(snapPoint, draggable){
    draggable.remove();
    createNodeShape(draggable.parentPile.template, snapPoint);
    updateConnections();
}

function globalPos(mouseEvent){
    return {x: mouseEvent.clientX + window.scrollX, y: mouseEvent.clientY + window.scrollY}
}

function updateDraggablePosition(mouseEvent){
    if (!isDragging || !currentDraggable) return;
    
    let g = globalPos(mouseEvent);

    const left = g.x - draggableHalfSize.w
    const top = g.y - draggableHalfSize.h

    currentDraggable.element.style.left = `${left}px`;
    currentDraggable.element.style.top = `${top}px`;

    checkSnap(left, top);
}

frag.appendChild(document.querySelector(".draggable-pile"));
frag.appendChild(document.querySelector(".draggable"));
const draggablePileElement = frag.querySelector(".draggable-pile");
const draggableElement = frag.querySelector(".draggable");

fetch('functionList.json')
.then(response => response.json())
.then(data => {
    let arrowSection = document.getElementById("arrow-section");

    data.data.forEach(e => {
        let pile = new DraggablePile(e);
        arrowSection.appendChild(pile.element);
    })
    
})
.catch(error => console.error("Error fetching JSON data:", error));

//optime the performance of this
const resizeObserver = new ResizeObserver(function(mutations) {updateConnections()});
resizeObserver.observe(document.getElementById("comp-area"))

//-----------------------------------
//split updating by columns
//-----------------------------------
