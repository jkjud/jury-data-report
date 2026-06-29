import { Sankey } from "./Sankey";
import { juryData } from "./data";

export const App = () => {
  const handleNavigate = (target: string) => {
    // Hook this up to your router. For now, just log.
    // e.g. navigate(`/${target}`)
    console.log("navigate ->", target);
    alert(`Navigate to: ${target}`);
  };

  return (
    <div className="page">
      <h2 className="title">Jury Pipeline</h2>
      <div className="sankey-wrap">
        <Sankey width={1040} height={620} data={juryData} onNavigate={handleNavigate} />
      </div>
    </div>
  );
};
