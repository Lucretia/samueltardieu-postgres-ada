with Ada.Unchecked_Deallocation;

package body PGAda.Results is
   use type ICS.Chars_Ptr;
   use type PGAda.Thin.PG_Result_Access;

   Exec_Status_Match : constant array (Thin.Exec_Status) of Results.Exec_Status :=
     (PGAda.Thin.PGRES_EMPTY_QUERY    => Results.Empty_Query,
      PGAda.Thin.PGRES_COMMAND_OK     => Results.Command_OK,
      PGAda.Thin.PGRES_TUPLES_OK      => Results.Tuples_OK,
      PGAda.Thin.PGRES_COPY_OUT       => Results.Copy_Out,
      PGAda.Thin.PGRES_COPY_IN        => Results.Copy_In,
      PGAda.Thin.PGRES_BAD_RESPONSE   => Results.Bad_Response,
      PGAda.Thin.PGRES_NONFATAL_ERROR => Results.Non_Fatal_Error,
      PGAda.Thin.PGRES_FATAL_ERROR    => Results.Fatal_Error);

   ----------------
   -- Error_Code --
   ----------------

   function Error_Code (Result : in Results.Result) return PGAda.Errors.Error_ID is
   begin
      return PGAda.Errors.Value (Results.Result_Error_Field (Result, PGAda.Thin.PG_DIAG_SQLSTATE) );
   end Error_Code;

   ----------------------
   -- Number_Of_Fields --
   ----------------------

   function Number_Of_Fields (Result : in Results.Result) return Natural is
   begin
      return Natural (PGAda.Thin.PQ_N_Fields (Result.Actual) );
   end Number_Of_Fields;

   ----------------------
   -- Number_Of_Tuples --
   ----------------------

   function Number_Of_Tuples (Result : in Results.Result) return Natural is
   begin
      return Natural (PGAda.Thin.PQ_N_Tuples (Result.Actual) );
   end Number_Of_Tuples;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Result : in out Results.Result) is
   begin
      Result.Ref_Count.all := Result.Ref_Count.all + 1;
   end Adjust;

   -----------
   -- Clear --
   -----------

   procedure Clear (Result : in out Results.Result) is
   begin
      PGAda.Thin.PQ_Clear (Result.Actual);
      Result.Actual := null;
   end Clear;

   ------------
   -- Status --
   ------------

   function Status (Result : in Results.Result) return Exec_Status is
   begin
      return Exec_Status_Match (PGAda.Thin.PQ_Result_Status (Result.Actual) );
   end Status;

   --------------------
   -- Command_Status --
   --------------------

   function Command_Status (Result : in Results.Result) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Cmd_Status (Result.Actual) );
   end Command_Status;

   --------------------
   -- Command_Tuples --
   --------------------

   function Command_Tuples (Result : in Results.Result) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Cmd_Tuples (Result.Actual) );
   end Command_Tuples;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Result : Results.Result) return String is
   begin
      return Result_Error_Field (Result => Result, Field => PGAda.Thin.PG_DIAG_MESSAGE_PRIMARY);
   end Error_Message;

   ------------------------
   -- Result_Error_Field --
   ------------------------

   function Result_Error_Field
     (Result : in Results.Result;
      Field  : in Error_Field)
   return String is
      C_Res : constant ICS.chars_ptr := PGAda.Thin.PQ_Result_Error_Field (Result.Actual, Field);
   begin
      if C_Res = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (C_Res);
      end if;
   end Result_Error_Field;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Result : in out Results.Result) is
      procedure Free is new Ada.Unchecked_Deallocation (Natural, Natural_Access);
   begin
      Result.Ref_Count.all := Result.Ref_Count.all - 1;

      if Result.Ref_Count.all = 0 and then Result.Actual /= null then
         Free (Result.Ref_Count);
         Clear (Result);
      end if;
  end Finalize;

end PGAda.Results;
