interface Props {
  className?: string;
}

const LandingSupport = ({ className }: Props) => {
  return (
    <div className="flex flex-col items-center mt-20 mb-20 gap-8 mx-4 text-center">
      <h2 className="text-3xl font-bold">We&apos;re here to help.</h2>
      <div>
        Email us at{" "}
        <a className="underline" href="mailto:aisupport@appdirect.com">
          aisupport@appdirect.com
        </a>{" "}
        if you have any questions.
      </div>
    </div>
  );
};

export default LandingSupport;
