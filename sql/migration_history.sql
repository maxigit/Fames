-- Update history
update fames_stocktake
set history= "[]"
where history = ""

update fames_boxtake
set location_history= "[]"
where location_history = ""
