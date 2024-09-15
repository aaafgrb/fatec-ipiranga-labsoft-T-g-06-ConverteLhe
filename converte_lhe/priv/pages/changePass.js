async function changePass(){
  const pass = document.getElementById("passTxt");

  //todo: make password checking

  const feedback = document.getElementById("feedbackLbl")

  let params = new URLSearchParams(document.location.search);
  let key = params.get("k");

  if(key == null){
      feedback.innerHTML = "password change failed: invalid reset key";
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