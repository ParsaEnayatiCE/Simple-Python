from tkinter import *
from tkinter import messagebox
from tkinter import filedialog
import subprocess
import os


class TextEditor:
    def __init__(self, root):
        self.root = root
        self.root.title("simplified python IDE")
        self.root.geometry("1000x600+200+150")
        self.filename = None
        self.title = StringVar()
        self.status = StringVar()
        self.titlebar = Label(self.root, textvariable=self.title, font=("times new roman", 15, "bold"), bd=2, relief=GROOVE)
        self.titlebar.pack(side=TOP,fill=BOTH)
        self.settitle()
        self.statusbar = Button(self.root, textvariable=self.status, font=("times new roman", 15, "bold"), bd=2, relief=GROOVE)
        self.statusbar.pack(side=BOTTOM, fill=BOTH)
        self.statusbar.bind("<Button-1>", self.run_code)
        self.status.set("Run Code")
        self.menubar = Menu(self.root, font=("times new roman", 15, "bold"), activebackground="skyblue")
        self.root.config(menu=self.menubar)
        self.filemenu = Menu(self.menubar, font=("times new roman", 12), activebackground="skyblue", tearoff=0)
        self.filemenu.add_command(label="New", accelerator="Ctrl+N", command=self.newfile)
        self.filemenu.add_command(label="Open", accelerator="Ctrl+O", command=self.openfile)
        self.filemenu.add_command(label="Save", accelerator="Ctrl+S", command=self.savefile)
        self.filemenu.add_command(label="Save As", accelerator="Ctrl+A", command=self.saveasfile)
        self.filemenu.add_separator()
        self.filemenu.add_command(label="Exit", accelerator="Ctrl+E", command=self.exit)
        self.menubar.add_cascade(label="File", menu=self.filemenu)
        self.editmenu = Menu(self.menubar, font=("times new roman", 12), activebackground="skyblue", tearoff=0)
        self.editmenu.add_command(label="Cut", accelerator="Ctrl+X", command=self.cut)
        self.editmenu.add_command(label="Copy", accelerator="Ctrl+C", command=self.copy)
        self.editmenu.add_command(label="Paste", accelerator="Ctrl+V", command=self.paste)
        self.menubar.add_cascade(label="Edit", menu=self.editmenu)
        self.viewmenu = Menu(self.menubar, font=("times new roman", 12), activebackground="skyblue", tearoff=0)
        self.viewmenu.add_command(label="Zoom in", accelerator="Ctrl+\'+\'", command=self.zoom_in)
        self.viewmenu.add_command(label="Zoom out", accelerator="Ctrl+\'-\'", command=self.zoom_out)
        self.menubar.add_cascade(label="View", menu=self.viewmenu)
        scrol_y = Scrollbar(self.root, orient=VERTICAL)
        self.txtarea = Text(self.root, yscrollcommand=scrol_y.set, font=("times new roman", 11), state="normal", relief=GROOVE)
        scrol_y.pack(side=RIGHT, fill=Y)
        scrol_y.config(command=self.txtarea.yview)
        self.txtarea.pack(fill=BOTH, expand=1)
        self.shortcuts()

    def settitle(self):
        if self.filename:
            self.title.set(self.filename)
        else:
            self.title.set("Untitled")

    def newfile(self, *args):
        self.txtarea.delete("1.0", END)
        self.filename = None
        self.settitle()

    def openfile(self, *args):
        try:
            self.filename = filedialog.askopenfilename(title="Select file", filetypes=(("Simplified Python Files", "*.spy"), ("Text Files", "*.txt")))
            if self.filename:
                infile = open(self.filename, "r")
                self.txtarea.delete("1.0", END)
                for line in infile:
                    self.txtarea.insert(END, line)
                infile.close()
                self.settitle()
        except Exception as e:
            pass

    def savefile(self,*args):
        try:
            if self.filename:
                data = self.txtarea.get("1.0", END)
                outfile = open(self.filename, "w")
                outfile.write(data)
                outfile.close()
                self.settitle()
            else:
                self.saveasfile()
        except Exception as e:
            pass

    def saveasfile(self, *args):
        try:
            untitledfile = filedialog.asksaveasfilename(title="Save file As", defaultextension=".spy", initialfile="Untitled.spy", filetypes=(("Simplified Python Files", "*.spy"), ("Text Files", "*.txt")))
            data = self.txtarea.get("1.0", END)
            outfile = open(untitledfile, "w")
            outfile.write(data)
            outfile.close()
            self.filename = untitledfile
            self.settitle()
        except Exception as e:
            pass

    def exit(self, *args):
        op = messagebox.askyesno("WARNING", "Your Unsaved Data May be Lost! Are you Sure?")
        if op > 0:
            self.root.destroy()
        else:
            return

    def cut(self, *args):
        self.txtarea.event_generate("<Cut>")

    def copy(self, *args):
        self.txtarea.event_generate("<Copy>")

    def paste(self, *args):
        self.txtarea.event_generate("<Paste>")

    def zoom_in(self, *args):
        font = self.txtarea.cget('font').split("} ")
        font[0] = font[0][1:]
        new_font = (font[0], int(font[1]) + 1)
        self.txtarea.configure(font=new_font)

    def zoom_out(self, *args):
        font = self.txtarea.cget('font').split("} ")
        font[0] = font[0][1:]
        new_font = (font[0], int(font[1]) - 1)
        self.txtarea.configure(font=new_font)

    def shortcuts(self):
        self.txtarea.bind("<Control-n>", self.newfile)
        self.txtarea.bind("<Control-o>", self.openfile)
        self.txtarea.bind("<Control-s>", self.savefile)
        self.txtarea.bind("<Control-a>", self.saveasfile)
        self.txtarea.bind("<Control-e>", self.exit)
        self.txtarea.bind("<Control-x>", self.cut)
        self.txtarea.bind("<Control-c>", self.copy)
        self.txtarea.bind("<Control-v>", self.paste)
        self.txtarea.bind("<Control-equal>", self.zoom_in)
        self.txtarea.bind("<Control-minus>", self.zoom_out)

    def run_code(self, *args):
        if self.filename:
            code = open(self.filename, 'r').read()
        else:
            code = self.txtarea.get("1.0",END)
        script_file = open("execute.sh", 'w')
        script_file.write(f"!#/bin/bash\nracket execute.rkt -a \"{code}\"\nread -p \"Press Enter to continue...\"")
        script_file.close()
        subprocess.call("execute.sh", shell=True)
        os.remove("execute.sh")


root = Tk()
TextEditor(root)
root.mainloop()
