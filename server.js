const express = require("express");
const app = express();
const port = 80;

app.use(express.static("build"));

app.listen(port);
