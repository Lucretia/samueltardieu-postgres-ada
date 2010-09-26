
package body PGAda.Connections is
   use type PGAda.Thin.PG_Conn_Access;

   -------------
   -- Connect --
   -------------

   procedure Connect (Connection : in out Connections.Connection; Info : String) is
      C_Info : ICS.chars_ptr := C_String_Or_Null (Info);
   begin
      Connection.Actual := PGAda.Thin.PQ_Connect_DB (C_Info);

      Free (C_Info);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Connection : in out Connections.Connection;
       Host       : in String  := "";
       Port       : in Natural := 0;
       Options    : in String  := "";
       TTY        : in String  := "";
       DB_Name    : in String  := "";
       Login      : in String  := "";
       Password   : in String  := "")
   is
      C_Host     : ICS.chars_ptr := C_String_Or_Null (Host);
      C_Port     : ICS.chars_ptr;
      C_Options  : ICS.chars_ptr := C_String_Or_Null (Options);
      C_TTY      : ICS.chars_ptr := C_String_Or_Null (TTY);
      C_DB_Name  : ICS.chars_ptr := C_String_Or_Null (DB_Name);
      C_Login    : ICS.chars_ptr := C_String_Or_Null (Login);
      C_Password : ICS.chars_ptr := C_String_Or_Null (Password);
   begin
      if Port = 0 then
         C_Port := ICS.Null_Ptr;
      else
         C_Port := ICS.New_String (Positive'Image (Port) );
      end if;

      Connection.Actual :=
         PGAda.Thin.PQ_Set_Db_Login
           (C_Host,
            C_Port,
            C_Options,
            C_TTY,
            C_DB_Name,
            C_Login,
            C_Password);

      Free (C_Host);
      Free (C_Port);
      Free (C_Options);
      Free (C_TTY);
      Free (C_DB_Name);
      Free (C_Login);
      Free (C_Password);

      if Connection.Actual = null then
         raise PG_Error;
      end if;
   end Connect;

   ------------
   -- Status --
   ------------

   function Status (Connection : in Connections.Connection) return Connection_Status is
   begin
      case PGAda.Thin.PQ_Status (Connection.Actual) is
         when PGAda.Thin.CONNECTION_OK  => return Connection_OK;
         when PGAda.Thin.CONNECTION_BAD => return Connection_Bad;
      end case;
   end Status;

   --------
   -- DB --
   --------

   function Database (Connection : in Connections.Connection) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Db (Connection.Actual) );
   end Database;

  ----------
  -- Host --
  ----------

  function Host (Connection : in Connections.Connection) return String is
  begin
    return ICS.Value (PGAda.Thin.PQ_Host (Connection.Actual) );
  end Host;

  -------------------
  -- Error_Message --
  -------------------

  function Error_Message (Connection : in Connections.Connection) return String is
  begin
    return ICS.Value (PGAda.Thin.PQ_Error_Message (Connection.Actual));
  end Error_Message;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Connection : in out Connections.Connection) is
   begin
      if Connection.Actual /= null then
         Connections.Finish (Connection);
      end if;
   end Finalize;

   ------------
   -- Finish --
   ------------

   procedure Finish (Connection : in out Connections.Connection) is
   begin
      PGAda.Thin.PQ_Finish (Connection.Actual);
      Connection.Actual := null;
   end Finish;

   -------------
   -- Options --
   -------------

   function Options (Connection : in Connections.Connection) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Options (Connection.Actual) );
   end Options;

   ----------
   -- Port --
   ----------

   function Port (Connection : in Connections.Connection) return Positive is
   begin
      return Positive'Value (ICS.Value (PGAda.Thin.PQ_Port (Connection.Actual) ));
   end Port;

   -----------
   -- Reset --
   -----------

   procedure Reset (Connection : in Connections.Connection) is
   begin
      PGAda.Thin.PQ_Reset (Connection.Actual);
   end Reset;

end PGAda.Connections;
