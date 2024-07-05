#include "TextEditor.h"

static bool TokenizeCStyleString(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	if (*p == '"')
	{
		p++;

		while (p < in_end)
		{
			// handle end of string
			if (*p == '"')
			{
				out_begin = in_begin;
				out_end = p + 1;
				return true;
			}

			// handle escape character for "
			if (*p == '\\' && p + 1 < in_end && p[1] == '"')
				p++;

			p++;
		}
	}

	return false;
}

static bool TokenizeCStyleCharacterLiteral(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	if (*p == '\'')
	{
		p++;

		// handle escape characters
		if (p < in_end && *p == '\\')
			p++;

		if (p < in_end)
			p++;

		// handle end of character literal
		if (p < in_end && *p == '\'')
		{
			out_begin = in_begin;
			out_end = p + 1;
			return true;
		}
	}

	return false;
}

static bool TokenizeCStyleIdentifier(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || *p == '_')
	{
		p++;

		while ((p < in_end) && ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_'))
			p++;

		out_begin = in_begin;
		out_end = p;
		return true;
	}

	return false;
}


static bool TokenizeX86StyleNumber(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
    const char* p = in_begin;

    const bool startsWithNumber = *p >= '0' && *p <= '9';

    if (*p != '+' && *p != '-' && !startsWithNumber)
        return false;

    p++;

    bool hasNumber = startsWithNumber;

    while (p < in_end && (*p >= '0' && *p <= '9'))
    {
        hasNumber = true;

        p++;
    }

    if (hasNumber == false)
        return false;

    bool isFloat = false;
    bool isHex = false;
    bool isBinary = false;

    if (p < in_end)
    {
        if (*p == '.')
        {
            isFloat = true;
            p++;

            while (p < in_end && (*p >= '0' && *p <= '9'))
                p++;
        }
        else if (*p == 'x' || *p == 'X')
        {
            // hex formatted integer of the type 0xef80

            isHex = true;

            p++;

            while (p < in_end && ((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f') || (*p >= 'A' && *p <= 'F')))
                p++;
        }
    }

    out_begin = in_begin;
    out_end = p;
    return true;
}

static bool TokenizeCStylePunctuation(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	(void)in_end;

	switch (*in_begin)
	{
	case '[':
	case ']':
	case '{':
	case '}':
	case '!':
	case '%':
	case '^':
	case '&':
	case '*':
	case '(':
	case ')':
	case '-':
	case '+':
	case '=':
	case '~':
	case '|':
	case '<':
	case '>':
	case '?':
	case ':':
	case '/':
	case ';':
	case ',':
	case '.':
		out_begin = in_begin;
		out_end = in_begin + 1;
		return true;
	}

	return false;
}


static bool TokenizeCStyleNumber(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
    const char* p = in_begin;

    const bool startsWithNumber = *p >= '0' && *p <= '9';

    if (*p != '+' && *p != '-' && !startsWithNumber)
        return false;

    p++;

    bool hasNumber = startsWithNumber;

    while (p < in_end && (*p >= '0' && *p <= '9'))
    {
        hasNumber = true;

        p++;
    }

    if (hasNumber == false)
        return false;

    bool isFloat = false;
    bool isHex = false;
    bool isBinary = false;

    if (p < in_end)
    {
        if (*p == '.')
        {
            isFloat = true;

            p++;

            while (p < in_end && (*p >= '0' && *p <= '9'))
                p++;
        }
        else if (*p == 'x' || *p == 'X')
        {
            // hex formatted integer of the type 0xef80

            isHex = true;

            p++;

            while (p < in_end && ((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f') || (*p >= 'A' && *p <= 'F')))
                p++;
        }
        else if (*p == 'b' || *p == 'B')
        {
            // binary formatted integer of the type 0b01011101

            isBinary = true;

            p++;

            while (p < in_end && (*p >= '0' && *p <= '1'))
                p++;
        }
    }

    if (isHex == false && isBinary == false)
    {
        // floating point exponent
        if (p < in_end && (*p == 'e' || *p == 'E'))
        {
            isFloat = true;

            p++;

            if (p < in_end && (*p == '+' || *p == '-'))
                p++;

            bool hasDigits = false;

            while (p < in_end && (*p >= '0' && *p <= '9'))
            {
                hasDigits = true;

                p++;
            }

            if (hasDigits == false)
                return false;
        }

        // single precision floating point type
        if (p < in_end && *p == 'f')
            p++;
    }

    if (isFloat == false)
    {
        // integer size type
        while (p < in_end && (*p == 'u' || *p == 'U' || *p == 'l' || *p == 'L'))
            p++;
    }

    out_begin = in_begin;
    out_end = p;
    return true;
}

static bool TokenizeX86StylePunctuation(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
    (void)in_end;

    switch (*in_begin)
    {
        case '[':
        case ']':
        case '*':
        case '(':  // at&t syntax
        case ')':
        case '-':
        case '+':
        case ':':
            out_begin = in_begin;
            out_end = in_begin + 1;
            return true;
    }

    return false;
}

static bool TokenizeLuaStyleString(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	bool is_single_quote = false;
	bool is_double_quotes = false;
	bool is_double_square_brackets = false;

	switch (*p)
	{
	case '\'':
		is_single_quote = true;
		break;
	case '"':
		is_double_quotes = true;
		break;
	case '[':
		p++;
		if (p < in_end && *(p) == '[')
			is_double_square_brackets = true;
		break;
	}

	if (is_single_quote || is_double_quotes || is_double_square_brackets)
	{
		p++;

		while (p < in_end)
		{
			// handle end of string
			if ((is_single_quote && *p == '\'') || (is_double_quotes && *p == '"') || (is_double_square_brackets && *p == ']' && p + 1 < in_end && *(p + 1) == ']'))
			{
				out_begin = in_begin;

				if (is_double_square_brackets)
					out_end = p + 2;
				else
					out_end = p + 1;

				return true;
			}

			// handle escape character for "
			if (*p == '\\' && p + 1 < in_end && (is_single_quote || is_double_quotes))
				p++;

			p++;
		}
	}

	return false;
}

static bool TokenizeLuaStyleIdentifier(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || *p == '_')
	{
		p++;

		while ((p < in_end) && ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_'))
			p++;

		out_begin = in_begin;
		out_end = p;
		return true;
	}

	return false;
}

static bool TokenizeLuaStyleNumber(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	const char* p = in_begin;

	const bool startsWithNumber = *p >= '0' && *p <= '9';

	if (*p != '+' && *p != '-' && !startsWithNumber)
		return false;

	p++;

	bool hasNumber = startsWithNumber;

	while (p < in_end && (*p >= '0' && *p <= '9'))
	{
		hasNumber = true;

		p++;
	}

	if (hasNumber == false)
		return false;

	if (p < in_end)
	{
		if (*p == '.')
		{
			p++;

			while (p < in_end && (*p >= '0' && *p <= '9'))
				p++;
		}

		// floating point exponent
		if (p < in_end && (*p == 'e' || *p == 'E'))
		{
			p++;

			if (p < in_end && (*p == '+' || *p == '-'))
				p++;

			bool hasDigits = false;

			while (p < in_end && (*p >= '0' && *p <= '9'))
			{
				hasDigits = true;

				p++;
			}

			if (hasDigits == false)
				return false;
		}
	}

	out_begin = in_begin;
	out_end = p;
	return true;
}

static bool TokenizeLuaStylePunctuation(const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end)
{
	(void)in_end;

	switch (*in_begin)
	{
	case '[':
	case ']':
	case '{':
	case '}':
	case '!':
	case '%':
	case '#':
	case '^':
	case '&':
	case '*':
	case '(':
	case ')':
	case '-':
	case '+':
	case '=':
	case '~':
	case '|':
	case '<':
	case '>':
	case '?':
	case ':':
	case '/':
	case ';':
	case ',':
	case '.':
		out_begin = in_begin;
		out_end = in_begin + 1;
		return true;
	}

	return false;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Asmx86_64()
{
    static bool inited = false;
    static LanguageDefinition langDef;
    if (!inited)
    {
        static const char* const x86Keywords[] =  {"aaa", "aad", "aam", "aas", "fabs", "adc", "adcx", "add", "addpd", "addps", "addsd", "addss", "addsubpd", "addsubps", "fadd", "fiadd", "adox", "aesdeclast", "aesdec", "aesenclast", "aesenc", "aesimc", "aeskeygenassist", "and", "andn", "andnpd", "andnps", "andpd", "andps", "arpl", "bextr", "blcfill", "blci", "blcic", "blcmsk", "blcs", "blendpd", "blendps", "blendvpd", "blendvps", "blsfill", "blsi", "blsic", "blsmsk", "blsr", "bndcl", "bndcn", "bndcu", "bndldx", "bndmk", "bndmov", "bndstx", "bound", "bsf", "bsr", "bswap", "bt", "btc", "btr", "bts", "bzhi", "call", "cbw", "cdq", "cdqe", "fchs", "clac", "clc", "cld", "cldemote", "clflush", "clflushopt", "clgi", "cli", "clrssbsy", "clts", "clwb", "clzero", "cmc", "cmova", "cmovae", "cmovb", "cmovbe", "fcmovbe", "fcmovb", "cmove", "fcmove", "cmovg", "cmovge", "cmovl", "cmovle", "fcmovnbe", "fcmovnb", "cmovne", "fcmovne", "cmovno", "cmovnp", "fcmovnu", "fcmovnp", "cmovns", "cmovo", "cmovp", "fcmovu", "cmovs", "cmp", "cmppd", "cmpps", "cmpsb", "cmpsd", "cmpsq", "cmpss", "cmpsw", "cmpxchg16b", "cmpxchg", "cmpxchg8b", "comisd", "comiss", "fcomp", "fcompi", "fcomi", "fcom", "fcos", "cpuid", "cqo", "crc32", "cvtdq2pd", "cvtdq2ps", "cvtpd2dq", "cvtpd2ps", "cvtps2dq", "cvtps2pd", "cvtsd2si", "cvtsd2ss", "cvtsi2sd", "cvtsi2ss", "cvtss2sd", "cvtss2si", "cvttpd2dq", "cvttps2dq", "cvttsd2si", "cvttss2si", "cwd", "cwde", "daa", "das", "data16", "dec", "div", "divpd", "divps", "fdivr", "fidivr", "fdivrp", "divsd", "divss", "fdiv", "fidiv", "fdivp", "dppd", "dpps", "encls", "enclu", "enclv", "endbr32", "endbr64", "enter", "extractps", "extrq", "f2xm1", "lcall", "ljmp", "jmp", "fbld", "fbstp", "fcompp", "fdecstp", "fdisi8087_nop", "femms", "feni8087_nop", "ffree", "ffreep", "ficom", "ficomp", "fincstp", "fldcw", "fldenv", "fldl2e", "fldl2t", "fldlg2", "fldln2", "fldpi", "fnclex", "fninit", "fnop", "fnstcw", "fnstsw", "fpatan", "fstpnce", "fprem", "fprem1", "fptan", "frndint", "frstor", "fnsave", "fscale", "fsetpm", "fsincos", "fnstenv", "fxam", "fxrstor", "fxrstor64", "fxsave", "fxsave64", "fxtract", "fyl2x", "fyl2xp1", "getsec", "gf2p8affineinvqb", "gf2p8affineqb", "gf2p8mulb", "haddpd", "haddps", "hlt", "hsubpd", "hsubps", "idiv", "fild", "imul", "in", "inc", "incsspd", "incsspq", "insb", "insertps", "insertq", "insd", "insw", "int", "int1", "int3", "into", "invd", "invept", "invlpg", "invlpga", "invpcid", "invvpid", "iret", "iretd", "iretq", "fisttp", "fist", "fistp", "jae", "ja", "jbe", "jb", "jcxz", "jecxz", "je", "jge", "jg", "jle", "jl", "jne", "jno", "jnp", "jns", "jo", "jp", "jrcxz", "js", "kaddb", "kaddd", "kaddq", "kaddw", "kandb", "kandd", "kandnb", "kandnd", "kandnq", "kandnw", "kandq", "kandw", "kmovb", "kmovd", "kmovq", "kmovw", "knotb", "knotd", "knotq", "knotw", "korb", "kord", "korq", "kortestb", "kortestd", "kortestq", "kortestw", "korw", "kshiftlb", "kshiftld", "kshiftlq", "kshiftlw", "kshiftrb", "kshiftrd", "kshiftrq", "kshiftrw", "ktestb", "ktestd", "ktestq", "ktestw", "kunpckbw", "kunpckdq", "kunpckwd", "kxnorb", "kxnord", "kxnorq", "kxnorw", "kxorb", "kxord", "kxorq", "kxorw", "lahf", "lar", "lddqu", "ldmxcsr", "lds", "fldz", "fld1", "fld", "lea", "leave", "les", "lfence", "lfs", "lgdt", "lgs", "lidt", "lldt", "llwpcb", "lmsw", "lock", "lodsb", "lodsd", "lodsq", "lodsw", "loop", "loope", "loopne", "retf", "retfq", "lsl", "lss", "ltr", "lwpins", "lwpval", "lzcnt", "maskmovdqu", "maxpd", "maxps", "maxsd", "maxss", "mfence", "minpd", "minps", "minsd", "minss", "cvtpd2pi", "cvtpi2pd", "cvtpi2ps", "cvtps2pi", "cvttpd2pi", "cvttps2pi", "emms", "maskmovq", "movd", "movq", "movdq2q", "movntq", "movq2dq", "pabsb", "pabsd", "pabsw", "packssdw", "packsswb", "packuswb", "paddb", "paddd", "paddq", "paddsb", "paddsw", "paddusb", "paddusw", "paddw", "palignr", "pandn", "pand", "pavgb", "pavgw", "pcmpeqb", "pcmpeqd", "pcmpeqw", "pcmpgtb", "pcmpgtd", "pcmpgtw", "pextrw", "phaddd", "phaddsw", "phaddw", "phsubd", "phsubsw", "phsubw", "pinsrw", "pmaddubsw", "pmaddwd", "pmaxsw", "pmaxub", "pminsw", "pminub", "pmovmskb", "pmulhrsw", "pmulhuw", "pmulhw", "pmullw", "pmuludq", "por", "psadbw", "pshufb", "pshufw", "psignb", "psignd", "psignw", "pslld", "psllq", "psllw", "psrad", "psraw", "psrld", "psrlq", "psrlw", "psubb", "psubd", "psubq", "psubsb", "psubsw", "psubusb", "psubusw", "psubw", "punpckhbw", "punpckhdq", "punpckhwd", "punpcklbw", "punpckldq", "punpcklwd", "pxor", "monitorx", "monitor", "montmul", "mov", "movabs", "movapd", "movaps", "movbe", "movddup", "movdir64b", "movdiri", "movdqa", "movdqu", "movhlps", "movhpd", "movhps", "movlhps", "movlpd", "movlps", "movmskpd", "movmskps", "movntdqa", "movntdq", "movnti", "movntpd", "movntps", "movntsd", "movntss", "movsb", "movsd", "movshdup", "movsldup", "movsq", "movss", "movsw", "movsx", "movsxd", "movupd", "movups", "movzx", "mpsadbw", "mul", "mulpd", "mulps", "mulsd", "mulss", "mulx", "fmul", "fimul", "fmulp", "mwaitx", "mwait", "neg", "nop", "not", "or", "orpd", "orps", "out", "outsb", "outsd", "outsw", "packusdw", "pause", "pavgusb", "pblendvb", "pblendw", "pclmulqdq", "pcmpeqq", "pcmpestri", "pcmpestrm", "pcmpgtq", "pcmpistri", "pcmpistrm", "pconfig", "pdep", "pext", "pextrb", "pextrd", "pextrq", "pf2id", "pf2iw", "pfacc", "pfadd", "pfcmpeq", "pfcmpge", "pfcmpgt", "pfmax", "pfmin", "pfmul", "pfnacc", "pfpnacc", "pfrcpit1", "pfrcpit2", "pfrcp", "pfrsqit1", "pfrsqrt", "pfsubr", "pfsub", "phminposuw", "pi2fd", "pi2fw", "pinsrb", "pinsrd", "pinsrq", "pmaxsb", "pmaxsd", "pmaxud", "pmaxuw", "pminsb", "pminsd", "pminud", "pminuw", "pmovsxbd", "pmovsxbq", "pmovsxbw", "pmovsxdq", "pmovsxwd", "pmovsxwq", "pmovzxbd", "pmovzxbq", "pmovzxbw", "pmovzxdq", "pmovzxwd", "pmovzxwq", "pmuldq", "pmulhrw", "pmulld", "pop", "popaw", "popal", "popcnt", "popf", "popfd", "popfq", "prefetch", "prefetchnta", "prefetcht0", "prefetcht1", "prefetcht2", "prefetchw", "prefetchwt1", "pshufd", "pshufhw", "pshuflw", "pslldq", "psrldq", "pswapd", "ptest", "ptwrite", "punpckhqdq", "punpcklqdq", "push", "pushaw", "pushal", "pushf", "pushfd", "pushfq", "rcl", "rcpps", "rcpss", "rcr", "rdfsbase", "rdgsbase", "rdmsr", "rdpid", "rdpkru", "rdpmc", "rdrand", "rdseed", "rdsspd", "rdsspq", "rdtsc", "rdtscp", "repne", "rep", "ret", "rex64", "rol", "ror", "rorx", "roundpd", "roundps", "roundsd", "roundss", "rsm", "rsqrtps", "rsqrtss", "rstorssp", "sahf", "sal", "salc", "sar", "sarx", "saveprevssp", "sbb", "scasb", "scasd", "scasq", "scasw", "setae", "seta", "setbe", "setb", "sete", "setge", "setg", "setle", "setl", "setne", "setno", "setnp", "setns", "seto", "setp", "setssbsy", "sets", "sfence", "sgdt", "sha1msg1", "sha1msg2", "sha1nexte", "sha1rnds4", "sha256msg1", "sha256msg2", "sha256rnds2", "shl", "shld", "shlx", "shr", "shrd", "shrx", "shufpd", "shufps", "sidt", "fsin", "skinit", "sldt", "slwpcb", "smsw", "sqrtpd", "sqrtps", "sqrtsd", "sqrtss", "fsqrt", "stac", "stc", "std", "stgi", "sti", "stmxcsr", "stosb", "stosd", "stosq", "stosw", "str", "fst", "fstp", "sub", "subpd", "subps", "fsubr", "fisubr", "fsubrp", "subsd", "subss", "fsub", "fisub", "fsubp", "swapgs", "syscall", "sysenter", "sysexit", "sysexitq", "sysret", "sysretq", "t1mskc", "test", "tpause", "ftst", "tzcnt", "tzmsk", "ucomisd", "ucomiss", "fucompi", "fucomi", "fucompp", "fucomp", "fucom", "ud0", "ud1", "ud2", "umonitor", "umwait", "unpckhpd", "unpckhps", "unpcklpd", "unpcklps", "v4fmaddps", "v4fmaddss", "v4fnmaddps", "v4fnmaddss", "vaddpd", "vaddps", "vaddsd", "vaddss", "vaddsubpd", "vaddsubps", "vaesdeclast", "vaesdec", "vaesenclast", "vaesenc", "vaesimc", "vaeskeygenassist", "valignd", "valignq", "vandnpd", "vandnps", "vandpd", "vandps", "vblendmpd", "vblendmps", "vblendpd", "vblendps", "vblendvpd", "vblendvps", "vbroadcastf128", "vbroadcastf32x2", "vbroadcastf32x4", "vbroadcastf32x8", "vbroadcastf64x2", "vbroadcastf64x4", "vbroadcasti128", "vbroadcasti32x2", "vbroadcasti32x4", "vbroadcasti32x8", "vbroadcasti64x2", "vbroadcasti64x4", "vbroadcastsd", "vbroadcastss", "vcmp", "vcmppd", "vcmpps", "vcmpsd", "vcmpss", "vcomisd", "vcomiss", "vcompresspd", "vcompressps", "vcvtdq2pd", "vcvtdq2ps", "vcvtpd2dq", "vcvtpd2ps", "vcvtpd2qq", "vcvtpd2udq", "vcvtpd2uqq", "vcvtph2ps", "vcvtps2dq", "vcvtps2pd", "vcvtps2ph", "vcvtps2qq", "vcvtps2udq", "vcvtps2uqq", "vcvtqq2pd", "vcvtqq2ps", "vcvtsd2si", "vcvtsd2ss", "vcvtsd2usi", "vcvtsi2sd", "vcvtsi2ss", "vcvtss2sd", "vcvtss2si", "vcvtss2usi", "vcvttpd2dq", "vcvttpd2qq", "vcvttpd2udq", "vcvttpd2uqq", "vcvttps2dq", "vcvttps2qq", "vcvttps2udq", "vcvttps2uqq", "vcvttsd2si", "vcvttsd2usi", "vcvttss2si", "vcvttss2usi", "vcvtudq2pd", "vcvtudq2ps", "vcvtuqq2pd", "vcvtuqq2ps", "vcvtusi2sd", "vcvtusi2ss", "vdbpsadbw", "vdivpd", "vdivps", "vdivsd", "vdivss", "vdppd", "vdpps", "verr", "verw", "vexp2pd", "vexp2ps", "vexpandpd", "vexpandps", "vextractf128", "vextractf32x4", "vextractf32x8", "vextractf64x2", "vextractf64x4", "vextracti128", "vextracti32x4", "vextracti32x8", "vextracti64x2", "vextracti64x4", "vextractps", "vfixupimmpd", "vfixupimmps", "vfixupimmsd", "vfixupimmss", "vfmadd132pd", "vfmadd132ps", "vfmadd132sd", "vfmadd132ss", "vfmadd213pd", "vfmadd213ps", "vfmadd213sd", "vfmadd213ss", "vfmadd231pd", "vfmadd231ps", "vfmadd231sd", "vfmadd231ss", "vfmaddpd", "vfmaddps", "vfmaddsd", "vfmaddss", "vfmaddsub132pd", "vfmaddsub132ps", "vfmaddsub213pd", "vfmaddsub213ps", "vfmaddsub231pd", "vfmaddsub231ps", "vfmaddsubpd", "vfmaddsubps", "vfmsub132pd", "vfmsub132ps", "vfmsub132sd", "vfmsub132ss", "vfmsub213pd", "vfmsub213ps", "vfmsub213sd", "vfmsub213ss", "vfmsub231pd", "vfmsub231ps", "vfmsub231sd", "vfmsub231ss", "vfmsubadd132pd", "vfmsubadd132ps", "vfmsubadd213pd", "vfmsubadd213ps", "vfmsubadd231pd", "vfmsubadd231ps", "vfmsubaddpd", "vfmsubaddps", "vfmsubpd", "vfmsubps", "vfmsubsd", "vfmsubss", "vfnmadd132pd", "vfnmadd132ps", "vfnmadd132sd", "vfnmadd132ss", "vfnmadd213pd", "vfnmadd213ps", "vfnmadd213sd", "vfnmadd213ss", "vfnmadd231pd", "vfnmadd231ps", "vfnmadd231sd", "vfnmadd231ss", "vfnmaddpd", "vfnmaddps", "vfnmaddsd", "vfnmaddss", "vfnmsub132pd", "vfnmsub132ps", "vfnmsub132sd", "vfnmsub132ss", "vfnmsub213pd", "vfnmsub213ps", "vfnmsub213sd", "vfnmsub213ss", "vfnmsub231pd", "vfnmsub231ps", "vfnmsub231sd", "vfnmsub231ss", "vfnmsubpd", "vfnmsubps", "vfnmsubsd", "vfnmsubss", "vfpclasspd", "vfpclassps", "vfpclasssd", "vfpclassss", "vfrczpd", "vfrczps", "vfrczsd", "vfrczss", "vgatherdpd", "vgatherdps", "vgatherpf0dpd", "vgatherpf0dps", "vgatherpf0qpd", "vgatherpf0qps", "vgatherpf1dpd", "vgatherpf1dps", "vgatherpf1qpd", "vgatherpf1qps", "vgatherqpd", "vgatherqps", "vgetexppd", "vgetexpps", "vgetexpsd", "vgetexpss", "vgetmantpd", "vgetmantps", "vgetmantsd", "vgetmantss", "vgf2p8affineinvqb", "vgf2p8affineqb", "vgf2p8mulb", "vhaddpd", "vhaddps", "vhsubpd", "vhsubps", "vinsertf128", "vinsertf32x4", "vinsertf32x8", "vinsertf64x2", "vinsertf64x4", "vinserti128", "vinserti32x4", "vinserti32x8", "vinserti64x2", "vinserti64x4", "vinsertps", "vlddqu", "vldmxcsr", "vmaskmovdqu", "vmaskmovpd", "vmaskmovps", "vmaxpd", "vmaxps", "vmaxsd", "vmaxss", "vmcall", "vmclear", "vmfunc", "vminpd", "vminps", "vminsd", "vminss", "vmlaunch", "vmload", "vmmcall", "vmovq", "vmovapd", "vmovaps", "vmovddup", "vmovd", "vmovdqa32", "vmovdqa64", "vmovdqa", "vmovdqu16", "vmovdqu32", "vmovdqu64", "vmovdqu8", "vmovdqu", "vmovhlps", "vmovhpd", "vmovhps", "vmovlhps", "vmovlpd", "vmovlps", "vmovmskpd", "vmovmskps", "vmovntdqa", "vmovntdq", "vmovntpd", "vmovntps", "vmovsd", "vmovshdup", "vmovsldup", "vmovss", "vmovupd", "vmovups", "vmpsadbw", "vmptrld", "vmptrst", "vmread", "vmresume", "vmrun", "vmsave", "vmulpd", "vmulps", "vmulsd", "vmulss", "vmwrite", "vmxoff", "vmxon", "vorpd", "vorps", "vp4dpwssds", "vp4dpwssd", "vpabsb", "vpabsd", "vpabsq", "vpabsw", "vpackssdw", "vpacksswb", "vpackusdw", "vpackuswb", "vpaddb", "vpaddd", "vpaddq", "vpaddsb", "vpaddsw", "vpaddusb", "vpaddusw", "vpaddw", "vpalignr", "vpandd", "vpandnd", "vpandnq", "vpandn", "vpandq", "vpand", "vpavgb", "vpavgw", "vpblendd", "vpblendmb", "vpblendmd", "vpblendmq", "vpblendmw", "vpblendvb", "vpblendw", "vpbroadcastb", "vpbroadcastd", "vpbroadcastmb2q", "vpbroadcastmw2d", "vpbroadcastq", "vpbroadcastw", "vpclmulqdq", "vpcmov", "vpcmp", "vpcmpb", "vpcmpd", "vpcmpeqb", "vpcmpeqd", "vpcmpeqq", "vpcmpeqw", "vpcmpestri", "vpcmpestrm", "vpcmpgtb", "vpcmpgtd", "vpcmpgtq", "vpcmpgtw", "vpcmpistri", "vpcmpistrm", "vpcmpq", "vpcmpub", "vpcmpud", "vpcmpuq", "vpcmpuw", "vpcmpw", "vpcom", "vpcomb", "vpcomd", "vpcompressb", "vpcompressd", "vpcompressq", "vpcompressw", "vpcomq", "vpcomub", "vpcomud", "vpcomuq", "vpcomuw", "vpcomw", "vpconflictd", "vpconflictq", "vpdpbusds", "vpdpbusd", "vpdpwssds", "vpdpwssd", "vperm2f128", "vperm2i128", "vpermb", "vpermd", "vpermi2b", "vpermi2d", "vpermi2pd", "vpermi2ps", "vpermi2q", "vpermi2w", "vpermil2pd", "vpermilpd", "vpermil2ps", "vpermilps", "vpermpd", "vpermps", "vpermq", "vpermt2b", "vpermt2d", "vpermt2pd", "vpermt2ps", "vpermt2q", "vpermt2w", "vpermw", "vpexpandb", "vpexpandd", "vpexpandq", "vpexpandw", "vpextrb", "vpextrd", "vpextrq", "vpextrw", "vpgatherdd", "vpgatherdq", "vpgatherqd", "vpgatherqq", "vphaddbd", "vphaddbq", "vphaddbw", "vphadddq", "vphaddd", "vphaddsw", "vphaddubd", "vphaddubq", "vphaddubw", "vphaddudq", "vphadduwd", "vphadduwq", "vphaddwd", "vphaddwq", "vphaddw", "vphminposuw", "vphsubbw", "vphsubdq", "vphsubd", "vphsubsw", "vphsubwd", "vphsubw", "vpinsrb", "vpinsrd", "vpinsrq", "vpinsrw", "vplzcntd", "vplzcntq", "vpmacsdd", "vpmacsdqh", "vpmacsdql", "vpmacssdd", "vpmacssdqh", "vpmacssdql", "vpmacsswd", "vpmacssww", "vpmacswd", "vpmacsww", "vpmadcsswd", "vpmadcswd", "vpmadd52huq", "vpmadd52luq", "vpmaddubsw", "vpmaddwd", "vpmaskmovd", "vpmaskmovq", "vpmaxsb", "vpmaxsd", "vpmaxsq", "vpmaxsw", "vpmaxub", "vpmaxud", "vpmaxuq", "vpmaxuw", "vpminsb", "vpminsd", "vpminsq", "vpminsw", "vpminub", "vpminud", "vpminuq", "vpminuw", "vpmovb2m", "vpmovd2m", "vpmovdb", "vpmovdw", "vpmovm2b", "vpmovm2d", "vpmovm2q", "vpmovm2w", "vpmovmskb", "vpmovq2m", "vpmovqb", "vpmovqd", "vpmovqw", "vpmovsdb", "vpmovsdw", "vpmovsqb", "vpmovsqd", "vpmovsqw", "vpmovswb", "vpmovsxbd", "vpmovsxbq", "vpmovsxbw", "vpmovsxdq", "vpmovsxwd", "vpmovsxwq", "vpmovusdb", "vpmovusdw", "vpmovusqb", "vpmovusqd", "vpmovusqw", "vpmovuswb", "vpmovw2m", "vpmovwb", "vpmovzxbd", "vpmovzxbq", "vpmovzxbw", "vpmovzxdq", "vpmovzxwd", "vpmovzxwq", "vpmuldq", "vpmulhrsw", "vpmulhuw", "vpmulhw", "vpmulld", "vpmullq", "vpmullw", "vpmultishiftqb", "vpmuludq", "vpopcntb", "vpopcntd", "vpopcntq", "vpopcntw", "vpord", "vporq", "vpor", "vpperm", "vprold", "vprolq", "vprolvd", "vprolvq", "vprord", "vprorq", "vprorvd", "vprorvq", "vprotb", "vprotd", "vprotq", "vprotw", "vpsadbw", "vpscatterdd", "vpscatterdq", "vpscatterqd", "vpscatterqq", "vpshab", "vpshad", "vpshaq", "vpshaw", "vpshlb", "vpshldd", "vpshldq", "vpshldvd", "vpshldvq", "vpshldvw", "vpshldw", "vpshld", "vpshlq", "vpshlw", "vpshrdd", "vpshrdq", "vpshrdvd", "vpshrdvq", "vpshrdvw", "vpshrdw", "vpshufbitqmb", "vpshufb", "vpshufd", "vpshufhw", "vpshuflw", "vpsignb", "vpsignd", "vpsignw", "vpslldq", "vpslld", "vpsllq", "vpsllvd", "vpsllvq", "vpsllvw", "vpsllw", "vpsrad", "vpsraq", "vpsravd", "vpsravq", "vpsravw", "vpsraw", "vpsrldq", "vpsrld", "vpsrlq", "vpsrlvd", "vpsrlvq", "vpsrlvw", "vpsrlw", "vpsubb", "vpsubd", "vpsubq", "vpsubsb", "vpsubsw", "vpsubusb", "vpsubusw", "vpsubw", "vpternlogd", "vpternlogq", "vptestmb", "vptestmd", "vptestmq", "vptestmw", "vptestnmb", "vptestnmd", "vptestnmq", "vptestnmw", "vptest", "vpunpckhbw", "vpunpckhdq", "vpunpckhqdq", "vpunpckhwd", "vpunpcklbw", "vpunpckldq", "vpunpcklqdq", "vpunpcklwd", "vpxord", "vpxorq", "vpxor", "vrangepd", "vrangeps", "vrangesd", "vrangess", "vrcp14pd", "vrcp14ps", "vrcp14sd", "vrcp14ss", "vrcp28pd", "vrcp28ps", "vrcp28sd", "vrcp28ss", "vrcpps", "vrcpss", "vreducepd", "vreduceps", "vreducesd", "vreducess", "vrndscalepd", "vrndscaleps", "vrndscalesd", "vrndscaless", "vroundpd", "vroundps", "vroundsd", "vroundss", "vrsqrt14pd", "vrsqrt14ps", "vrsqrt14sd", "vrsqrt14ss", "vrsqrt28pd", "vrsqrt28ps", "vrsqrt28sd", "vrsqrt28ss", "vrsqrtps", "vrsqrtss", "vscalefpd", "vscalefps", "vscalefsd", "vscalefss", "vscatterdpd", "vscatterdps", "vscatterpf0dpd", "vscatterpf0dps", "vscatterpf0qpd", "vscatterpf0qps", "vscatterpf1dpd", "vscatterpf1dps", "vscatterpf1qpd", "vscatterpf1qps", "vscatterqpd", "vscatterqps", "vshuff32x4", "vshuff64x2", "vshufi32x4", "vshufi64x2", "vshufpd", "vshufps", "vsqrtpd", "vsqrtps", "vsqrtsd", "vsqrtss", "vstmxcsr", "vsubpd", "vsubps", "vsubsd", "vsubss", "vtestpd", "vtestps", "vucomisd", "vucomiss", "vunpckhpd", "vunpckhps", "vunpcklpd", "vunpcklps", "vxorpd", "vxorps", "vzeroall", "vzeroupper", "wait", "wbinvd", "wbnoinvd", "wrfsbase", "wrgsbase", "wrmsr", "wrpkru", "wrssd", "wrssq", "wrussd", "wrussq", "xabort", "xacquire", "xadd", "xbegin", "xchg", "fxch", "xcryptcbc", "xcryptcfb", "xcryptctr", "xcryptecb", "xcryptofb", "xend", "xgetbv", "xlatb", "xor", "xorpd", "xorps", "xrelease", "xrstor", "xrstor64", "xrstors", "xrstors64", "xsave", "xsave64", "xsavec", "xsavec64", "xsaveopt", "xsaveopt64", "xsaves", "xsaves64", "xsetbv", "xsha1", "xsha256", "xstore", "xtest"};

        for (auto& k : x86Keywords)
            langDef.mKeywords.insert(k);

        static const char* const identifiers[] = {
                // General-purpose registers (32-bit)
                "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
                "al", "ah", "bl", "bh", "cl", "ch", "dl", "dh",
                // General-purpose registers (64-bit)
                "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
                "sil", "dil", "bpl", "spl",
                "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
                // General-purpose registers (16-bit)
                "ax", "bx", "cx", "dx", "si", "di", "bp", "sp",
                // Segment registers
                "cs", "ds", "ss", "es", "fs", "gs",
                // Control registers
                "cr0", "cr1", "cr2", "cr3", "cr4", "cr8",
                "cr5", "cr6", "cr7", "cr9", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15",
                // Debug registers
                "dr0", "dr1", "dr2", "dr3", "dr6", "dr7",
                "dr4", "dr5",
                // Extended multimedia registers (SSE/AVX)
                "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
                "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15",
                // Floating-point registers
                "st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7",
                // Other registers
                "eip", "rip", "eflags", "rflags",
        };

        for (auto& k : identifiers)
        {
            Identifier id;
            id.mDeclaration = "CPU Register";
            langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
        }

        langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_]{1}[_a-zA-Z0-9]{0,}:)##", PaletteIndex::Identifier));

        langDef.mTokenize = [](const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end, PaletteIndex& paletteIndex) -> bool
        {
            paletteIndex = PaletteIndex::Max;

            while (in_begin < in_end && isascii(*in_begin) && isblank(*in_begin))
                in_begin++;

            if (in_begin == in_end)
            {
                out_begin = in_end;
                out_end = in_end;
                paletteIndex = PaletteIndex::Default;
            }
            else if (TokenizeCStyleString(in_begin, in_end, out_begin, out_end))
                paletteIndex = PaletteIndex::String;
            else if (TokenizeCStyleCharacterLiteral(in_begin, in_end, out_begin, out_end))
                paletteIndex = PaletteIndex::CharLiteral;
            else if (TokenizeCStyleIdentifier(in_begin, in_end, out_begin, out_end))
                paletteIndex = PaletteIndex::Identifier;
            else if (TokenizeCStyleNumber(in_begin, in_end, out_begin, out_end))
                paletteIndex = PaletteIndex::Number;
            else if (TokenizeCStylePunctuation(in_begin, in_end, out_begin, out_end))
                paletteIndex = PaletteIndex::Punctuation;

            return paletteIndex != PaletteIndex::Max;
        };

        langDef.mCommentStart = "%comment";
        langDef.mCommentEnd = "%endcomment";
        langDef.mSingleLineComment = ";";

        langDef.mCaseSensitive = true;

        langDef.mName = "Assembly x86";
        inited = true;
    }
    return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Cpp()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const cppKeywords[] = {
			"alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept", "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class",
			"compl", "concept", "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false", "float",
			"for", "friend", "goto", "if", "import", "inline", "int", "long", "module", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected", "public",
			"register", "reinterpret_cast", "requires", "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "synchronized", "template", "this", "thread_local",
			"throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
		};
		for (auto& k : cppKeywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"abort", "abs", "acos", "asin", "atan", "atexit", "atof", "atoi", "atol", "ceil", "clock", "cosh", "ctime", "div", "exit", "fabs", "floor", "fmod", "getchar", "getenv", "isalnum", "isalpha", "isdigit", "isgraph",
			"ispunct", "isspace", "isupper", "kbhit", "log10", "log2", "log", "memcmp", "modf", "pow", "printf", "sprintf", "snprintf", "putchar", "putenv", "puts", "rand", "remove", "rename", "sinh", "sqrt", "srand", "strcat", "strcmp", "strerror", "time", "tolower", "toupper",
			"std", "string", "vector", "map", "unordered_map", "set", "unordered_set", "min", "max"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}


		langDef.mTokenize = [](const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end, PaletteIndex& paletteIndex) -> bool
		{
			paletteIndex = PaletteIndex::Max;

			while (in_begin < in_end && isascii(*in_begin) && isblank(*in_begin))
				in_begin++;

			if (in_begin == in_end)
			{
				out_begin = in_end;
				out_end = in_end;
				paletteIndex = PaletteIndex::Default;
			}
			else if (TokenizeCStyleString(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::String;
			else if (TokenizeCStyleCharacterLiteral(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::CharLiteral;
			else if (TokenizeCStyleIdentifier(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Identifier;
			else if (TokenizeCStyleNumber(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Number;
			else if (TokenizeCStylePunctuation(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Punctuation;

			return paletteIndex != PaletteIndex::Max;
		};

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "C++";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Hlsl()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"AppendStructuredBuffer", "asm", "asm_fragment", "BlendState", "bool", "break", "Buffer", "ByteAddressBuffer", "case", "cbuffer", "centroid", "class", "column_major", "compile", "compile_fragment",
			"CompileShader", "const", "continue", "ComputeShader", "ConsumeStructuredBuffer", "default", "DepthStencilState", "DepthStencilView", "discard", "do", "double", "DomainShader", "dword", "else",
			"export", "extern", "false", "float", "for", "fxgroup", "GeometryShader", "groupshared", "half", "Hullshader", "if", "in", "inline", "inout", "InputPatch", "int", "interface", "line", "lineadj",
			"linear", "LineStream", "matrix", "min16float", "min10float", "min16int", "min12int", "min16uint", "namespace", "nointerpolation", "noperspective", "NULL", "out", "OutputPatch", "packoffset",
			"pass", "pixelfragment", "PixelShader", "point", "PointStream", "precise", "RasterizerState", "RenderTargetView", "return", "register", "row_major", "RWBuffer", "RWByteAddressBuffer", "RWStructuredBuffer",
			"RWTexture1D", "RWTexture1DArray", "RWTexture2D", "RWTexture2DArray", "RWTexture3D", "sample", "sampler", "SamplerState", "SamplerComparisonState", "shared", "snorm", "stateblock", "stateblock_state",
			"static", "string", "struct", "switch", "StructuredBuffer", "tbuffer", "technique", "technique10", "technique11", "texture", "Texture1D", "Texture1DArray", "Texture2D", "Texture2DArray", "Texture2DMS",
			"Texture2DMSArray", "Texture3D", "TextureCube", "TextureCubeArray", "true", "typedef", "triangle", "triangleadj", "TriangleStream", "uint", "uniform", "unorm", "unsigned", "vector", "vertexfragment",
			"VertexShader", "void", "volatile", "while",
			"bool1","bool2","bool3","bool4","double1","double2","double3","double4", "float1", "float2", "float3", "float4", "int1", "int2", "int3", "int4", "in", "out", "inout",
			"uint1", "uint2", "uint3", "uint4", "dword1", "dword2", "dword3", "dword4", "half1", "half2", "half3", "half4",
			"float1x1","float2x1","float3x1","float4x1","float1x2","float2x2","float3x2","float4x2",
			"float1x3","float2x3","float3x3","float4x3","float1x4","float2x4","float3x4","float4x4",
			"half1x1","half2x1","half3x1","half4x1","half1x2","half2x2","half3x2","half4x2",
			"half1x3","half2x3","half3x3","half4x3","half1x4","half2x4","half3x4","half4x4",
		};
		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"abort", "abs", "acos", "all", "AllMemoryBarrier", "AllMemoryBarrierWithGroupSync", "any", "asdouble", "asfloat", "asin", "asint", "asint", "asuint",
			"asuint", "atan", "atan2", "ceil", "CheckAccessFullyMapped", "clamp", "clip", "cos", "cosh", "countbits", "cross", "D3DCOLORtoUBYTE4", "ddx",
			"ddx_coarse", "ddx_fine", "ddy", "ddy_coarse", "ddy_fine", "degrees", "determinant", "DeviceMemoryBarrier", "DeviceMemoryBarrierWithGroupSync",
			"distance", "dot", "dst", "errorf", "EvaluateAttributeAtCentroid", "EvaluateAttributeAtSample", "EvaluateAttributeSnapped", "exp", "exp2",
			"f16tof32", "f32tof16", "faceforward", "firstbithigh", "firstbitlow", "floor", "fma", "fmod", "frac", "frexp", "fwidth", "GetRenderTargetSampleCount",
			"GetRenderTargetSamplePosition", "GroupMemoryBarrier", "GroupMemoryBarrierWithGroupSync", "InterlockedAdd", "InterlockedAnd", "InterlockedCompareExchange",
			"InterlockedCompareStore", "InterlockedExchange", "InterlockedMax", "InterlockedMin", "InterlockedOr", "InterlockedXor", "isfinite", "isinf", "isnan",
			"ldexp", "length", "lerp", "lit", "log", "log10", "log2", "mad", "max", "min", "modf", "msad4", "mul", "noise", "normalize", "pow", "printf",
			"Process2DQuadTessFactorsAvg", "Process2DQuadTessFactorsMax", "Process2DQuadTessFactorsMin", "ProcessIsolineTessFactors", "ProcessQuadTessFactorsAvg",
			"ProcessQuadTessFactorsMax", "ProcessQuadTessFactorsMin", "ProcessTriTessFactorsAvg", "ProcessTriTessFactorsMax", "ProcessTriTessFactorsMin",
			"radians", "rcp", "reflect", "refract", "reversebits", "round", "rsqrt", "saturate", "sign", "sin", "sincos", "sinh", "smoothstep", "sqrt", "step",
			"tan", "tanh", "tex1D", "tex1D", "tex1Dbias", "tex1Dgrad", "tex1Dlod", "tex1Dproj", "tex2D", "tex2D", "tex2Dbias", "tex2Dgrad", "tex2Dlod", "tex2Dproj",
			"tex3D", "tex3D", "tex3Dbias", "tex3Dgrad", "tex3Dlod", "tex3Dproj", "texCUBE", "texCUBE", "texCUBEbias", "texCUBEgrad", "texCUBElod", "texCUBEproj", "transpose", "trunc"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([ \t]*#[ \t]*[a-zA-Z_]+)##", PaletteIndex::Preprocessor));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(L?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(\'\\?[^\']\')##", PaletteIndex::CharLiteral));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "HLSL";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Glsl()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short",
			"signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary",
			"_Noreturn", "_Static_assert", "_Thread_local"
		};
		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"abort", "abs", "acos", "asin", "atan", "atexit", "atof", "atoi", "atol", "ceil", "clock", "cosh", "ctime", "div", "exit", "fabs", "floor", "fmod", "getchar", "getenv", "isalnum", "isalpha", "isdigit", "isgraph",
			"ispunct", "isspace", "isupper", "kbhit", "log10", "log2", "log", "memcmp", "modf", "pow", "putchar", "putenv", "puts", "rand", "remove", "rename", "sinh", "sqrt", "srand", "strcat", "strcmp", "strerror", "time", "tolower", "toupper"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([ \t]*#[ \t]*[a-zA-Z_]+)##", PaletteIndex::Preprocessor));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(L?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(\'\\?[^\']\')##", PaletteIndex::CharLiteral));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "GLSL";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Python()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"False", "await", "else", "import", "pass", "None", "break", "except", "in", "raise", "True", "class", "finally", "is", "return", "and", "continue", "for", "lambda", "try", "as", "def", "from", "nonlocal", "while", "assert", "del", "global", "not", "with", "async", "elif", "if", "or", "yield"
		};
		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"abs", "aiter", "all", "any", "anext", "ascii", "bin", "bool", "breakpoint", "bytearray", "bytes", "callable", "chr", "classmethod", "compile", "complex", "delattr", "dict", "dir", "divmod", "enumerate", "eval", "exec", "filter", "float", "format", "frozenset", "getattr", "globals", "hasattr", "hash", "help", "hex", "id", "input", "int", "isinstance", "issubclass", "iter", "len", "list", "locals", "map", "max", "memoryview", "min", "next", "object", "oct", "open", "ord", "pow", "print", "property", "range", "repr", "reversed", "round", "set", "setattr", "slice", "sorted", "staticmethod", "str", "sum", "super", "tuple", "type", "vars", "zip", "__import__"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##((b|u|f|r)?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##((b|u|f|r)?'(\\.|[^'])*')##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.\:])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "\"\"\"";
		langDef.mCommentEnd = "\"\"\"";
		langDef.mSingleLineComment = "#";

		langDef.mCaseSensitive = true;

		langDef.mName = "Python";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::C()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short",
			"signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary",
			"_Noreturn", "_Static_assert", "_Thread_local"
		};
		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"abort", "abs", "acos", "asin", "atan", "atexit", "atof", "atoi", "atol", "ceil", "clock", "cosh", "ctime", "div", "exit", "fabs", "floor", "fmod", "getchar", "getenv", "isalnum", "isalpha", "isdigit", "isgraph",
			"ispunct", "isspace", "isupper", "kbhit", "log10", "log2", "log", "memcmp", "modf", "pow", "putchar", "putenv", "puts", "rand", "remove", "rename", "sinh", "sqrt", "srand", "strcat", "strcmp", "strerror", "time", "tolower", "toupper"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenize = [](const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end, PaletteIndex& paletteIndex) -> bool
		{
			paletteIndex = PaletteIndex::Max;

			while (in_begin < in_end && isascii(*in_begin) && isblank(*in_begin))
				in_begin++;

			if (in_begin == in_end)
			{
				out_begin = in_end;
				out_end = in_end;
				paletteIndex = PaletteIndex::Default;
			}
			else if (TokenizeCStyleString(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::String;
			else if (TokenizeCStyleCharacterLiteral(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::CharLiteral;
			else if (TokenizeCStyleIdentifier(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Identifier;
			else if (TokenizeCStyleNumber(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Number;
			else if (TokenizeCStylePunctuation(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Punctuation;

			return paletteIndex != PaletteIndex::Max;
		};

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "C";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Sql()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"ADD", "EXCEPT", "PERCENT", "ALL", "EXEC", "PLAN", "ALTER", "EXECUTE", "PRECISION", "AND", "EXISTS", "PRIMARY", "ANY", "EXIT", "PRINT", "AS", "FETCH", "PROC", "ASC", "FILE", "PROCEDURE",
			"AUTHORIZATION", "FILLFACTOR", "PUBLIC", "BACKUP", "FOR", "RAISERROR", "BEGIN", "FOREIGN", "READ", "BETWEEN", "FREETEXT", "READTEXT", "BREAK", "FREETEXTTABLE", "RECONFIGURE",
			"BROWSE", "FROM", "REFERENCES", "BULK", "FULL", "REPLICATION", "BY", "FUNCTION", "RESTORE", "CASCADE", "GOTO", "RESTRICT", "CASE", "GRANT", "RETURN", "CHECK", "GROUP", "REVOKE",
			"CHECKPOINT", "HAVING", "RIGHT", "CLOSE", "HOLDLOCK", "ROLLBACK", "CLUSTERED", "IDENTITY", "ROWCOUNT", "COALESCE", "IDENTITY_INSERT", "ROWGUIDCOL", "COLLATE", "IDENTITYCOL", "RULE",
			"COLUMN", "IF", "SAVE", "COMMIT", "IN", "SCHEMA", "COMPUTE", "INDEX", "SELECT", "CONSTRAINT", "INNER", "SESSION_USER", "CONTAINS", "INSERT", "SET", "CONTAINSTABLE", "INTERSECT", "SETUSER",
			"CONTINUE", "INTO", "SHUTDOWN", "CONVERT", "IS", "SOME", "CREATE", "JOIN", "STATISTICS", "CROSS", "KEY", "SYSTEM_USER", "CURRENT", "KILL", "TABLE", "CURRENT_DATE", "LEFT", "TEXTSIZE",
			"CURRENT_TIME", "LIKE", "THEN", "CURRENT_TIMESTAMP", "LINENO", "TO", "CURRENT_USER", "LOAD", "TOP", "CURSOR", "NATIONAL", "TRAN", "DATABASE", "NOCHECK", "TRANSACTION",
			"DBCC", "NONCLUSTERED", "TRIGGER", "DEALLOCATE", "NOT", "TRUNCATE", "DECLARE", "NULL", "TSEQUAL", "DEFAULT", "NULLIF", "UNION", "DELETE", "OF", "UNIQUE", "DENY", "OFF", "UPDATE",
			"DESC", "OFFSETS", "UPDATETEXT", "DISK", "ON", "USE", "DISTINCT", "OPEN", "USER", "DISTRIBUTED", "OPENDATASOURCE", "VALUES", "DOUBLE", "OPENQUERY", "VARYING","DROP", "OPENROWSET", "VIEW",
			"DUMMY", "OPENXML", "WAITFOR", "DUMP", "OPTION", "WHEN", "ELSE", "OR", "WHERE", "END", "ORDER", "WHILE", "ERRLVL", "OUTER", "WITH", "ESCAPE", "OVER", "WRITETEXT"
		};

		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"ABS", "ACOS", "ADD_MONTHS", "ASCII", "ASCIISTR", "ASIN", "ATAN", "ATAN2", "AVG", "BFILENAME", "BIN_TO_NUM", "BITAND", "CARDINALITY", "CASE", "CAST", "CEIL",
			"CHARTOROWID", "CHR", "COALESCE", "COMPOSE", "CONCAT", "CONVERT", "CORR", "COS", "COSH", "COUNT", "COVAR_POP", "COVAR_SAMP", "CUME_DIST", "CURRENT_DATE",
			"CURRENT_TIMESTAMP", "DBTIMEZONE", "DECODE", "DECOMPOSE", "DENSE_RANK", "DUMP", "EMPTY_BLOB", "EMPTY_CLOB", "EXP", "EXTRACT", "FIRST_VALUE", "FLOOR", "FROM_TZ", "GREATEST",
			"GROUP_ID", "HEXTORAW", "INITCAP", "INSTR", "INSTR2", "INSTR4", "INSTRB", "INSTRC", "LAG", "LAST_DAY", "LAST_VALUE", "LEAD", "LEAST", "LENGTH", "LENGTH2", "LENGTH4",
			"LENGTHB", "LENGTHC", "LISTAGG", "LN", "LNNVL", "LOCALTIMESTAMP", "LOG", "LOWER", "LPAD", "LTRIM", "MAX", "MEDIAN", "MIN", "MOD", "MONTHS_BETWEEN", "NANVL", "NCHR",
			"NEW_TIME", "NEXT_DAY", "NTH_VALUE", "NULLIF", "NUMTODSINTERVAL", "NUMTOYMINTERVAL", "NVL", "NVL2", "POWER", "RANK", "RAWTOHEX", "REGEXP_COUNT", "REGEXP_INSTR",
			"REGEXP_REPLACE", "REGEXP_SUBSTR", "REMAINDER", "REPLACE", "ROUND", "ROWNUM", "RPAD", "RTRIM", "SESSIONTIMEZONE", "SIGN", "SIN", "SINH",
			"SOUNDEX", "SQRT", "STDDEV", "SUBSTR", "SUM", "SYS_CONTEXT", "SYSDATE", "SYSTIMESTAMP", "TAN", "TANH", "TO_CHAR", "TO_CLOB", "TO_DATE", "TO_DSINTERVAL", "TO_LOB",
			"TO_MULTI_BYTE", "TO_NCLOB", "TO_NUMBER", "TO_SINGLE_BYTE", "TO_TIMESTAMP", "TO_TIMESTAMP_TZ", "TO_YMINTERVAL", "TRANSLATE", "TRIM", "TRUNC", "TZ_OFFSET", "UID", "UPPER",
			"USER", "USERENV", "VAR_POP", "VAR_SAMP", "VARIANCE", "VSIZE"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(L?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(\'[^\']*\')##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "--";

		langDef.mCaseSensitive = false;

		langDef.mName = "SQL";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::AngelScript()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"and", "abstract", "auto", "bool", "break", "case", "cast", "class", "const", "continue", "default", "do", "double", "else", "enum", "false", "final", "float", "for",
			"from", "funcdef", "function", "get", "if", "import", "in", "inout", "int", "interface", "int8", "int16", "int32", "int64", "is", "mixin", "namespace", "not",
			"null", "or", "out", "override", "private", "protected", "return", "set", "shared", "super", "switch", "this ", "true", "typedef", "uint", "uint8", "uint16", "uint32",
			"uint64", "void", "while", "xor"
		};

		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"cos", "sin", "tab", "acos", "asin", "atan", "atan2", "cosh", "sinh", "tanh", "log", "log10", "pow", "sqrt", "abs", "ceil", "floor", "fraction", "closeTo", "fpFromIEEE", "fpToIEEE",
			"complex", "opEquals", "opAddAssign", "opSubAssign", "opMulAssign", "opDivAssign", "opAdd", "opSub", "opMul", "opDiv"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(L?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(\'\\?[^\']\')##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "AngelScript";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Lua()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
		};

		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"assert", "collectgarbage", "dofile", "error", "getmetatable", "ipairs", "loadfile", "load", "loadstring", "next", "pairs", "pcall", "print", "rawequal", "rawlen", "rawget", "rawset",
			"select", "setmetatable", "tonumber", "tostring", "type", "xpcall", "_G", "_VERSION","arshift", "band", "bnot", "bor", "bxor", "btest", "extract", "lrotate", "lshift", "replace",
			"rrotate", "rshift", "create", "resume", "running", "status", "wrap", "yield", "isyieldable", "debug","getuservalue", "gethook", "getinfo", "getlocal", "getregistry", "getmetatable",
			"getupvalue", "upvaluejoin", "upvalueid", "setuservalue", "sethook", "setlocal", "setmetatable", "setupvalue", "traceback", "close", "flush", "input", "lines", "open", "output", "popen",
			"read", "tmpfile", "type", "write", "close", "flush", "lines", "read", "seek", "setvbuf", "write", "__gc", "__tostring", "abs", "acos", "asin", "atan", "ceil", "cos", "deg", "exp", "tointeger",
			"floor", "fmod", "ult", "log", "max", "min", "modf", "rad", "random", "randomseed", "sin", "sqrt", "string", "tan", "type", "atan2", "cosh", "sinh", "tanh",
			"pow", "frexp", "ldexp", "log10", "pi", "huge", "maxinteger", "mininteger", "loadlib", "searchpath", "seeall", "preload", "cpath", "path", "searchers", "loaded", "module", "require", "clock",
			"date", "difftime", "execute", "exit", "getenv", "remove", "rename", "setlocale", "time", "tmpname", "byte", "char", "dump", "find", "format", "gmatch", "gsub", "len", "lower", "match", "rep",
			"reverse", "sub", "upper", "pack", "packsize", "unpack", "concat", "maxn", "insert", "pack", "unpack", "remove", "move", "sort", "offset", "codepoint", "char", "len", "codes", "charpattern",
			"coroutine", "table", "io", "os", "string", "utf8", "bit32", "math", "debug", "package"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}

		langDef.mTokenize = [](const char* in_begin, const char* in_end, const char*& out_begin, const char*& out_end, PaletteIndex& paletteIndex) -> bool
		{
			paletteIndex = PaletteIndex::Max;

			while (in_begin < in_end && isascii(*in_begin) && isblank(*in_begin))
				in_begin++;

			if (in_begin == in_end)
			{
				out_begin = in_end;
				out_end = in_end;
				paletteIndex = PaletteIndex::Default;
			}
			else if (TokenizeLuaStyleString(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::String;
			else if (TokenizeLuaStyleIdentifier(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Identifier;
			else if (TokenizeLuaStyleNumber(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Number;
			else if (TokenizeLuaStylePunctuation(in_begin, in_end, out_begin, out_end))
				paletteIndex = PaletteIndex::Punctuation;

			return paletteIndex != PaletteIndex::Max;
		};

		langDef.mCommentStart = "--[[";
		langDef.mCommentEnd = "]]";
		langDef.mSingleLineComment = "--";

		langDef.mCaseSensitive = true;

		langDef.mName = "Lua";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Cs()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		static const char* const keywords[] = {
			"abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "in (generic modifier)", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "out (generic modifier)", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "using static", "void", "volatile", "while"
		};
		for (auto& k : keywords)
			langDef.mKeywords.insert(k);

		static const char* const identifiers[] = {
			"add", "alias", "ascending", "async", "await", "descending", "dynamic", "from", "get", "global", "group", "into", "join", "let", "orderby", "partial", "remove", "select", "set", "value", "var", "when", "where", "yield"
		};
		for (auto& k : identifiers)
		{
			Identifier id;
			id.mDeclaration = "Built-in function";
			langDef.mIdentifiers.insert(std::make_pair(std::string(k), id));
		}
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(($|@)?\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?[fF]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?[0-9]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[0-7]+[Uu]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(0[xX][0-9a-fA-F]+[uU]?[lL]?[lL]?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([a-zA-Z_][a-zA-Z0-9_]*)##", PaletteIndex::Identifier));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.])##", PaletteIndex::Punctuation));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "C#";

		inited = true;
	}
	return langDef;
}

const TextEditor::LanguageDefinition& TextEditor::LanguageDefinition::Json()
{
	static bool inited = false;
	static LanguageDefinition langDef;
	if (!inited)
	{
		langDef.mKeywords.clear();
		langDef.mIdentifiers.clear();

		
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(\"(\\.|[^\"])*\")##", PaletteIndex::String));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?)##", PaletteIndex::Number));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##([\[\]\{\}\!\%\^\&\*\(\)\-\+\=\~\|\<\>\?\/\;\,\.\:])##", PaletteIndex::Punctuation));
		langDef.mTokenRegexStrings.push_back(std::make_pair<std::string, PaletteIndex>(R"##(false|true)##", PaletteIndex::Keyword));

		langDef.mCommentStart = "/*";
		langDef.mCommentEnd = "*/";
		langDef.mSingleLineComment = "//";

		langDef.mCaseSensitive = true;

		langDef.mName = "Json";

		inited = true;
	}
	return langDef;
}
