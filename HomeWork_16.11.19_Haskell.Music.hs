type Album = String
type Group = String
data Song = Song { nameSong :: String,
                   group :: Group,
                   album :: Album}
                   deriving Show
data User = User { name:: String,
                   songs:: [Song],
                   albums:: [Album],
                   groups:: [Group]}
                   deriving Show


addSong :: User -> Song -> User
addSong (User myName mySongs myAlbums myGroups) (Song newSongName newGroup newAlbum) =
    User myName (mySongs++[Song newSongName newGroup newAlbum]) (myAlbums++[newAlbum]) (myGroups++[newGroup])

addAlbum :: User -> Album -> User
addAlbum (User myName mySongs myAlbums myGroups) newAlbum = User myName mySongs (myAlbums++[newAlbum]) myGroups


addGroup :: User -> Group -> User
addGroup (User myName mySongs myAlbums myGroups) newGroup = User myName mySongs myAlbums (myGroups++[newGroup])

-- Examples
x :: User
x = User "lol" [Song "The Scientist" "Coldplay" "The Scientist", Song "Deutchland" "Rammstein" "Deutchland"] ["vvv","Deutchland"] ["Coldplay", "Rammstein"]
xLovedSong :: Song
xLovedSong = Song "Midnight City" "M83" "Hurry up, We're Dreaming"