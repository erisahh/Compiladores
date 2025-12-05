.class public Prog
.super java/lang/Object
.field private static scanner Ljava/util/Scanner;

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static maior(DD)D

	.limit stack 15
	.limit locals 9

dload_0
dload_2
dcmpg
ifgt l0
	goto l1
l0:
dload_0
d2i
istore 4
	goto l2
l1:
dload_2
d2i
istore 4
l2:
iload 4
i2d
dreturn
.end method
.method public static somatorio(I)I

	.limit stack 15
	.limit locals 5

iconst_0
i2d
dstore_1
iconst_0
istore_3
l3:
iload_3
iload_0
if_icmplt l4
	goto l5
l4:
dload_1
iload_3
i2d
dadd
dstore_1
iload_3
iconst_1
iadd
istore_3
	goto l3
l5:
dload_1
d2i
ireturn
.end method
.method public static fat(I)I

	.limit stack 15
	.limit locals 3

iconst_0
istore_1
l6:
iload_0
iconst_0
if_icmpgt l7
	goto l8
l7:
iload_1
iload_0
imul
istore_1
iload_0
iconst_1
isub
istore_0
	goto l6
l8:
iload_1
ireturn
.end method
.method public static imprimir(Ljava/lang/String;D)V

	.limit stack 15
	.limit locals 6

getstatic java/lang/System/out Ljava/io/PrintStream;
aload_0
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
dload_1
invokevirtual java/io/PrintStream/println(D)V
return
.end method
.method public static main([Ljava/lang/String;)V
	.limit stack 15
	.limit locals 5

new java/util/Scanner
dup
getstatic java/lang/System/in Ljava/io/InputStream;
invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
putstatic Prog/scanner Ljava/util/Scanner;
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Numero:"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
getstatic Prog/scanner Ljava/util/Scanner;
invokevirtual java/util/Scanner/nextInt()I
istore_3
ldc2_w 4.5
d2i
invokestatic Prog/fat(I)I
istore 4
ldc2_w 2.5
bipush 10
i2d
invokestatic Prog/maior(DD)D
dstore_1
ldc "teste:"
iconst_1
i2d
invokestatic Prog/imprimir(Ljava/lang/String;D)V
return
	return
.end method

