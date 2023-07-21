unit Application.DependencyInjection;

interface

uses
  MVCFramework,
  MVCFramework.Controllers.Register,
  Spring.Container,
  Spring,
  Spring.Container.ActivatorExtension;


procedure RegisterDependencies;

procedure RegisterController(AController: TMVCControllerClazz);

implementation

uses
  Application.Controllers.Customer,
  Application.Services.Customer,
  Domain.Interfaces.Customer,
  Infrastructure.DbContext.Interfaces,
  Infrastructure.DbContext,
  Infrastructure.Repositories.Customer,
  Application.DependencyInjection.SingletonPerScope;


procedure RegisterDependencies;
begin
  GlobalContainer.AddExtension<TActivatorContainerExtension>();

  // Controllers
  RegisterController(TCustomerController);

  // Services
  GlobalContainer.RegisterType<ICustomerService, TCustomerService>
    .AsCustom(TSingletonPerScopeLifetimeManagerRepository.GetInstance('CustomerService'));

  // DbContext
  GlobalContainer.RegisterType<IDBContext, TDBContextFireDAC>
    .AsCustom(TSingletonPerScopeLifetimeManagerRepository.GetInstance('DbContext'));

  // Repositories
  GlobalContainer.RegisterType<ICustomerRepository, TCustomerRepository>
    .AsCustom(TSingletonPerScopeLifetimeManagerRepository.GetInstance('CustomerRepository'));

end;

procedure RegisterController(AController: TMVCControllerClazz);
begin
  GlobalContainer.RegisterType(AController.ClassInfo);

  TControllersRegister.Instance.RegisterController(AController,
    function: TMVCController
    begin
      GlobalContainer.Resolve(AController.ClassInfo).AsType(AController.ClassInfo, Result);
    end
    );

end;

end.
