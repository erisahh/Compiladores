.class public Prog
.super java/lang/Object
.field private static scanner Ljava/util/Scanner;

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static somatorio(I)I

	.limit stack 15
	.limit locals 5

iconst_0
i2d
dstore_1
iconst_0
istore_3
l0:
iload_3
iload_0
if_icmplt l1
	goto l2
l1:
dload_1
iload_3
i2d
dadd
dstore_1
iload_3
iconst_1
iadd
istore_3
	goto l0
l2:
dload_1
d2i
ireturn
.end method
.method public static main([Ljava/lang/String;)V
	.limit stack 15
	.limit locals 3

new java/util/Scanner
dup
getstatic java/lang/System/in Ljava/io/InputStream;
invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
putstatic Prog/scanner Ljava/util/Scanner;
getstatic Prog/scanner Ljava/util/Scanner;
invokevirtual java/util/Scanner/nextInt()I
istore_2
iload_2
invokestatic Prog/somatorio(I)I
istore_1
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_1
invokevirtual java/io/PrintStream/println(I)V
return
	return
.end method

