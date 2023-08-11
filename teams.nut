//array of tables
::teams <- []
//table of <playerid>=<team number>
::teamsByPlayers <- {}

/*
function moveEveryoneToTeam(iTeam) {
	for (local i = 1; i <= MaxPlayers ; i++)
	{
	    local player = PlayerInstanceFromIndex(i)
	    if (player == null) continue
	    movePlayerToTeam(player, iTeam)
	}	
}

function partitionCTFTeamToCustom(iTeam) {
	if (iTeam > 3) {
		printl("No can do. we only subdivide native teams.")
		return 1
	}
	// iterate throughout players and move those in team `iTeam` to custom team
	::MaxPlayers <- MaxClients().tointeger();
		
	for (local i = 1; i <= MaxPlayers ; i++) {
    local player = PlayerInstanceFromIndex(i)
    if (player == null) continue
    if (player.GetTeam() == iTeam) {
    	if (iPlayersMoved >= iPlayersPerTeam) {
    		iPlayersMoved = 0
    		iCurrentTeam++
    	}
    	movePlayerToTeam(player, 4+iCurrentTeam)
    	iPlayersMoved++
    }
	}	
}
*/

function createCustomTeam() {
	local iNewTeam = teams.len()
	teams.append({
		players = []
	})
	if (mapVars.br_team_highlight == 0)
		teams[iNewTeam].color <- RandomInt(0,255)+" "+RandomInt(0,255)+" "+RandomInt(0,255);
}

::ColorPlayer <- function(player, sColor) {
	player.__KeyValueFromString("rendercolor", sColor)
}

::QueueToCustomTeam <- function(pTarget) {
	// if the last team is full, create new team
	if ((teams.len() == 0) || (teams[teams.len() - 1].players.len() >= iPlayersPerTeam()))
 		createCustomTeam()
	if (!(pTarget in teamsByPlayers))
		pTarget.MoveToTeam( 3 + teams.len() )
}

//function TakeOverRecord
