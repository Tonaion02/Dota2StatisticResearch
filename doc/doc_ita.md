# DOCUMENTATION

## FILTERING FOR GAMEMODE AND GAMETYPE
Si è notato che la maggior parte delle entries del nostro dataset erano partite di gamemode 2 e con gametype che assumeva valori tra 2 e 3.
Questi valori si riferiscono alla tipologia di partita che è stata disputata, di conseguenza, si è deciso quindi di eliminare tutte quante le altre entries filtrando per gamemode = 2 e gametype = 2 oppure gametype = 3. Questa decisione è stata presa perchè, le partite riferendosi a modalità di gioco differenti, potrebbero avere meta del tutto differenti.

## AGGIUNGERE IL RESTO

## SCATTERPLOT PICKRATE AND WINRATE
Dopo aver costruito le feature pickrate e winrate per ciascun campione, si è deciso di plottare attraverso uno scatterplot il pickrate e winrate. Dopo aver disegnato lo scatterplot, si è notato che potrebbe avere senso dividere il grafico ottenuto in 4 parti:
- Angolo alto-sinistro:         rappresentano i campioni che hanno un basso pickrate e un alto winrate, un buon numero di campioni si trovano lì, "ci sono campioni che vengono pickati poco ma vincono tanto" 
- Angolo alto-destro:           rappresentano i campioni che hanno un alto pickrate un alto winrate, un buon numero di campioni si trovano lì, "ci sono campioni che vengono pickati tanto e vincono tanto"
- Angolo basso-sinistro:        rappresentano i campioni che hanno un basso pickrate e un basso winrate, pochi campioni si trovano lì, "ci sono pochi campioni che vengono pickati poco e vincono poco"
- Angolo basso-destro:          rappresentano i campioni che hanno un alto pickrate e un basso winrate, nessun campione si trova qui, "non ci sono campioni che hanno un alto pickrate ma un winrate basso"

Tutto senso rispetto a quello che sappiamo dei videogiochi
