const AuthLayout = ({
    children
  }: {
    children: React.ReactNode;
  }) => {
    return ( 
      <div className="blue-bg h-full">
        <div className="flex justify-center items-center">
          <h1 className="mt-5 text-2xl font-extrabold leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">AppDirect AI</h1>
        </div>
        <div className="flex justify-center items-center h-full">
          <div className="auth-side-panel">
            <h2 className="me-4 mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 md:text-5xl lg:text-6xl dark:text-white">Let AI change the way you work.</h2>
            <div>
              AppDirect gives you the ability to access AIs that help you be more successful in work and life.
              You can browse our catalog of community made AIs or create your own!
            </div>
          </div>
          {children}
        </div>
      </div>
     );
  }
   
  export default AuthLayout;