unit dbftojson;
{
  Does not currently support all data-types, I made this quickly for another
  project, if you wish to add new data-type exports to it, please provide
  a pull-request with your change, and I'd gladly accept it.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpjson, DateUtils;

function ExportDBF(DataSet: TDataSet): TJSONObject;

implementation

function ExportDBF(DataSet: TDataSet): TJSONObject;
var
  md, field: TJSONObject;
  data, fields: TJSONArray;
  i, oldrec: integer;
  fd: TFieldDef;
begin
  Result:=TJSONObject.Create;
  md:=TJSONObject.Create;
  data:=TJSONArray.Create;
  fields:=TJSONArray.Create;
  for i:=0 to DataSet.FieldCount-1 do
  begin
    fd:=DataSet.FieldDefs.Items[i];
    field:=TJSONObject.Create;
    field.Strings['name']:=fd.Name;
    if (fd.DataType = ftAutoInc) or (fd.DataType = ftInteger) then
      field.Strings['type']:='int'
    else if (fd.DataType = ftString) or (fd.DataType = ftMemo) then
      field.Strings['type']:='string'
    else if fd.DataType = ftDateTime then
      field.Strings['type']:='date';
    fields.Add(field);
  end;
  md.Add('fields', fields);
  md.Add('root', 'Data');
  with DataSet do
  begin
    oldrec:=RecNo;
    First;
    repeat
      field:=TJSONObject.Create;
      for i:=0 to DataSet.FieldCount-1 do
      begin
        fd:=DataSet.FieldDefs.Items[i];
        if (fd.DataType = ftAutoInc) or (fd.DataType = ftInteger) then
          field.Add(fd.Name, DataSet.Fields.Fields[i].AsInteger)
        else if (fd.DataType = ftString) or (fd.DataType = ftMemo) then
          field.Add(fd.Name, DataSet.Fields.Fields[i].AsString)
        else if fd.DataType = ftDateTime then
          field.Add(fd.Name, DateToISO8601(DataSet.Fields.Fields[i].AsDateTime));
      end;
      data.Add(field);
      Next;
    until EOF;
    RecNo:=oldrec;
  end;
  Result.Add('metaData', md);
  Result.Add('Data', data);
end;

end.

