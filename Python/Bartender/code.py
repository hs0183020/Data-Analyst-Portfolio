import win32com.client as win32

btApp = win32.Dispatch("BarTender.Application")
btApp.Visible = False

btFormat = btApp.Formats.Open("C:\\Users\\hsoriano\\Desktop\\YourLabel.btw", False, "PrinterName")