with Ada.Finalization;

with PGAda.Errors;
with PGAda.Thin;

package PGAda.Results is
   pragma Preelaborate;

   type Natural_Access is access Natural;

   type Result is new Ada.Finalization.Controlled with record
      Actual    : Thin.PG_Result_Access;
      Ref_Count : Natural_Access := new Integer'(1);
   end record;

   type Exec_Status is
     (Empty_Query,
      Command_OK,
      Tuples_OK,
      Copy_Out,
      Copy_In,
      Bad_Response,
      Non_Fatal_Error,
      Fatal_Error);

   subtype Error_Field is PGAda.Thin.Error_Field;

   procedure Clear (Result : in out Results.Result);

   function Error_Message (Result : in Results.Result) return String;

   function Status (Result : in Results.Result) return Exec_Status;

   function Command_Status (Result : in Results.Result) return String;

   function Command_Tuples (Result : in Results.Result) return String;

   function Result_Error_Field
     (Result : Results.Result;
      Field  : Error_Field)
   return String;

   function Error_Code (Result : Results.Result) return Errors.Error_ID;

   function Number_Of_Tuples (Result : Results.Result) return Natural;

   function Number_Of_Fields (Result : Results.Result) return Natural;

private

   procedure Adjust (Result : in out Results.Result);
   procedure Finalize (Result : in out Results.Result);

end PGAda.Results;
