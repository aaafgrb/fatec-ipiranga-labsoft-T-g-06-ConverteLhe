async function changePass(){
  const pass = document.getElementById("passTxt");
  const confirm = document.getElementById("confirmTxt");
  const feedback = document.getElementById("feedbackLbl")

  let params = new URLSearchParams(document.location.search);
  let key = params.get("k");

  if(key == null){
    feedback.innerHTML = "error: invalid reset key";
    return;
  }


  if(!/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d\w\W]{8,}$/.test(pass.value)){
    feedback.innerHTML = "error: password not strong enough | required: 1 uppercase letter; 1 lowercase letter; 1 digit; 8 characters long";
  }else{
    feedback.innerHTML = ""
  }

  if(pass.value != confirm.value){
    feedback.innerHTML = "error: passwords aren't equal!"
    return;
  }


  const obj = {
      req: "changePass",
      key: key,
      pass: await digestMessage(pass.value),
  };
  
  const response = fetch("./auseronn", {
    method: 'POST',
    body: JSON.stringify(obj),
  });

  feedback.innerHTML = "fetching...";

  response.then(async (res) => {
    let j = await res.json();
    feedback.innerHTML = j.error ? "error: " + j.error : "fetch resulted in: " + j.value;
  })
}

async function digestMessage(message) {
  const msgUint8 = new TextEncoder().encode(message); // encode as (utf-8) Uint8Array
  const hashBuffer = await window.crypto.subtle.digest("SHA-256", msgUint8); // hash the message
  const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
  const hashHex = hashArray
    .map((b) => b.toString(16).padStart(2, "0"))
    .join(""); // convert bytes to hex string
  return hashHex;
}