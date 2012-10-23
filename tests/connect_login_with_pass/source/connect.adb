with Ada.Text_IO;

with PGAda.Connections;

procedure Connect is
   Conn : PGAda.Connections.Connection;

   Status : PGAda.Connections.Connection_Status;

   use type PGAda.Connections.Connection_Status;

   Host     : constant String := "localhost";
   DB       : constant String := "test";
   Login    : constant String := "test";
   Password : constant String := "test";
begin
   PGAda.Connections.Connect
      (Connection => Conn,
       Host       => Host,
       DB_Name    => DB,
       Login      => Login,
       Password   => Password);

   Ada.Text_IO.Put_Line ("Connection Status => " & PGAda.Connections.Connection_Status'Image (PGAda.Connections.Status (Connection => Conn) ) );

   if Status = PGAda.Connections.Connection_Bad then
      Ada.Text_IO.Put_Line ("Error Message => " & PGAda.Connections.Error_Message (Connection => Conn) );
   end if;
end Connect;
