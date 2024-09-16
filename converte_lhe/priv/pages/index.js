let ckey = getCookie("convertelhekey");
if(ckey != ""){
  document.getElementById("apikeyTxt").value = ckey;
}


async function sendRequest() {
  const form = document.getElementById("inForm");
  const formData  = new FormData(form);
    
  //todo: insert composition data here
  //formData.append("test", "dataT");
  formData.append("comp", getComp())

  const response = await fetch("./form", {
    method: 'POST',
    body: formData
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

//------------------------------------------------------------------

function getCookie(cname) {
  let name = cname + "=";
  let decodedCookie = decodeURIComponent(document.cookie);
  let ca = decodedCookie.split(';');
  for(let i = 0; i <ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}