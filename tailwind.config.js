/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: ["class"],
  content: [
    "./pages/**/*.{ts,tsx}",
    "./components/**/*.{ts,tsx}",
    "./app/**/*.{ts,tsx}",
    "./src/**/*.{ts,tsx}",
  ],
  theme: {
    container: {
      center: true,
      padding: "2rem",
      screens: {
        "2xl": "1400px",
      },
    },
    extend: {
      colors: {
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        yellow: "hsl(var(--yellow))",
        orange: "hsl(var(--orange))",
        green: "hsl(var(--green))",
        background: "hsl(var(--background))",
        foreground: "hsl(var(--foreground))",
        profile: "hsl(var(--profile))",
        navy: "hsl(var(--navy))",
        navylight: "hsl(var(--navy-light))",
        sky: "hsl(var(--sky))",
        lime: "hsl(var(--lime))",
        primary: {
          DEFAULT: "hsl(var(--primary))",
          foreground: "hsl(var(--primary-foreground))",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      keyframes: {
        "accordion-down": {
          from: { height: 0 },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: 0 },
        },
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
      },
      containers: {
        "2xs": "16rem",
        "3xs": "12rem",
        "4xs": "8rem",
        "5xs": "4rem",
        "6xs": "2rem",
        "7xs": "1rem",
      },
      backgroundImage: {
        "unleash-pattern": "url('/unleash-bg.png')",
        "cta-pattern": "url('/cta-bg.png')",
      },
    },
    boxShadow: {
      glow: "0 0 25px 10px rgba(255, 255, 255, 0.15)",
    },
    fontFamily: {
      sans: ["Inter", "sans-serif"],
      serif: ["Roboto", "serif"],
    },
  },
  plugins: [
    require("tailwindcss-animate"),
    require("@tailwindcss/container-queries"),
  ],
};
