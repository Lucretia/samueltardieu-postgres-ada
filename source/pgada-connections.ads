with Ada.Finalization;

with PGAda.Thin;

package PGAda.Connections is
   pragma Preelaborate;

   type Connection is new Ada.Finalization.Limited_Controlled with record
      Actual : Thin.PG_Conn_Access;
   end record;

   type Connection_Status is (Connection_OK, Connection_Bad);

   -- Connect to a database
   procedure Connect (Connection : in out Connections.Connection; Info : String);

   procedure Connect
     (Connection : in out Connections.Connection;
      Host       : in String  := "";
      Port       : in Natural := 0;
      Options    : in String  := "";
      TTY        : in String  := "";
      DB_Name    : in String  := "";
      Login      : in String  := "";
      Password   : in String  := "");

   procedure Finish (Connection : in out Connections.Connection);
   procedure Reset  (Connection : in Connections.Connection);

   -- Query characteristics of an open connection
   function Database (Connection : in Connections.Connection) return String;
   function Host     (Connection : in Connections.Connection) return String;
   function Port     (Connection : in Connections.Connection) return Positive;
   function Options  (Connection : in Connections.Connection) return String;

   function Status        (Connection : in Connections.Connection) return Connection_Status;
   function Error_Message (Connection : in Connections.Connection) return String;

private

   procedure Finalize (Connection : in out Connections.Connection);

end PGAda.Connections;
