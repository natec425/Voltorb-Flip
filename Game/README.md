<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">

<div id="game"></div>
<script src="game.js"></script>
<script>
    var node = document.getElementById('game');
    var app = Elm.Game.Main.embed(node);
</script>