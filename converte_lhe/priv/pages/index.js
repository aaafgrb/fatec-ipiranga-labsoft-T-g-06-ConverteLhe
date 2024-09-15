async function sendRequest() {
  const form = document.getElementById("inForm");
  const formData  = new FormData(form);
    
  //todo: insert composition data here
  //formData.append("test", "dataT");

  const response = await fetch("./form", {
    method: 'POST',
    body: formData.append("comp", getComp())
  });

  //todo: show the results on the screen
  console.log(await response.json());
}

function userButton(){
  location.href="./user"
}

function shareButton(){
  
}

function getComp(){
  //hard coded for now
  return "x1/sapp/$concat/#2";
}